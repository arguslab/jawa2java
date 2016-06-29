/*
 * Copyright (c) 2016. Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Detailed contributors are listed in the CONTRIBUTOR.md
 */

package org.argus.jawa.java

import org.argus.jawa.compiler.lexer.Tokens._
import org.argus.jawa.compiler.parser.{Location, _}
import org.argus.jawa.core.{AccessFlag, JawaPackage, JawaType, Reporter}
import org.argus.jawa.core.io.SourceFile
import org.sireum.util._
import org.stringtemplate.v4.{ST, STGroupFile}


/**
  * Translate Jawa to Java.
  *
  * @author <a href="mailto:fgwei521@gmail.com">Fengguo Wei</a>
  * @author <a href="mailto:anwesh.tuladhar@gmail.com">Anwesh Tuladhar</a>
  */
class Jawa2Java(reporter: Reporter) {

  private val template = new STGroupFile("templates/JavaModel.stg")

  case class LocationIterator (locations: IList[Location]) {
    var pos = 0
    def next(): Location = {
      val current: Location = locations(pos)
      pos += 1
      current
    }

    def hasNext: Boolean = {
      pos < locations.length
    }

    def lookahead(): Location = {
      locations(pos)
    }
  }

  def translate(source: Either[String, SourceFile]): IMap[JawaType, String] = {
    JawaParser.parse[CompilationUnit](source, resolveBody = true, reporter) match {
      case Some(cu) =>
        visitCompilationUnit(cu)
      case None =>
        throw new Jawa2JavaTranslateException("ParserError")
    }
  }

  def visitCompilationUnit(cu: CompilationUnit): IMap[JawaType, String] = {
    cu.topDecls.map {
      case cid: ClassOrInterfaceDeclaration =>
        val classST = visitClassDeclaration(cid)
        cid.typ -> classST.render
    }.toMap
  }

  def visitClassDeclaration(cid: ClassOrInterfaceDeclaration): ST = {
    val imports: MSet[JawaType] = msetEmpty
    val cuTemplate = template.getInstanceOf("CompilationUnit")

    val pkgTemplate = template.getInstanceOf("Package")
    pkgTemplate.add("pkgName", cid.typ.getPackageName)
    cuTemplate.add("package", pkgTemplate)

    val classTemplate = template.getInstanceOf("ClassDecl")
    classTemplate.add("accessFlag", AccessFlag.toString(AccessFlag.getAccessFlags(cid.accessModifier)))
    classTemplate.add("className", cid.typ.simpleName)
    cuTemplate.add("classDecl", classTemplate)
    cid.extendsAndImplimentsClausesOpt match {
      case Some(clause) =>
        clause.superClassOpt match {
          case Some(superClass) =>
            imports += superClass
            classTemplate.add("exts", superClass.simpleName)
          case None =>
        }
        val implements: Array[String] = clause.interfaces.map {
          interface =>
            imports += interface        // Fully qualified name in interface?
            interface.simpleName
        }.toArray
        classTemplate.add("impls", implements)
      case None =>
    }

    val fieldTemplates: Array[ST] = cid.fields.map {
      field =>
        val fieldTemplate = visitFieldDeclaration(field, imports)
        fieldTemplate
    }.toArray
    classTemplate.add("fields", fieldTemplates)

    val methodTemplates: Array[ST] = cid.methods.map {
      method =>
        val methodTemplate = visitMethodDeclaration(method, imports)
        methodTemplate
    }.toArray
    classTemplate.add("methods", methodTemplates)

    val sortedImports: List[JawaType] = imports.toList filterNot(_.baseTyp == cid.typ.name) sortWith((x, y) => x.baseTyp < y.baseTyp)

    val importTemplates: Array[ST] = sortedImports.map {
      imp =>
        val importTemplate = template.getInstanceOf("Import")
        importTemplate.add("className", imp.baseTyp)
        importTemplate
    }.toArray
    cuTemplate.add("imports", importTemplates)
    cuTemplate
  }

  def visitFieldDeclaration(fd: Field with Declaration, imports: MSet[JawaType]): ST = {
    val fieldTemplate = template.getInstanceOf("FieldDecl")

    fieldTemplate.add("accessFlag", AccessFlag.toString(AccessFlag.getAccessFlags(fd.accessModifier)))

    fieldTemplate.add("attrTyp", fd.typ.typ.simpleName)
    fieldTemplate.add("attrName", fd.fieldName)
    addImport(fd.typ.typ, imports)

    //todo Need to check Instance or Static for translation??? Access Flag already determines it.
    /* fd match {
       case ifd: InstanceFieldDeclaration =>
         println ("instance field declaration: " + ifd.fieldName)
       case sfd: StaticFieldDeclaration =>
         println ("static field declaration: " + sfd.fieldName)
       case _ => println ("no match : " + fd.fieldName)
     }*/

    fieldTemplate
  }

  def visitMethodDeclaration(md: MethodDeclaration, imports: MSet[JawaType]): ST = {
    val methodTemplate = template.getInstanceOf("MethodDecl")
    val bodyStatements: MList[(Int, ST)] = mlistEmpty
    val isConstructor: Boolean = md.isConstructor

    if(isConstructor) {
      println ("is Constructor Declaration: " + md.signature.getClassName)
      println ("is Constructor Declaration: " + md.accessModifier)
      methodTemplate.add("accessFlag", AccessFlag.toString(AccessFlag.getAccessFlags(md.accessModifier)).replace("constructor", "").trim)
      methodTemplate.add("methodName", md.signature.classTyp.simpleName)
    } else {
      methodTemplate.add("accessFlag", AccessFlag.toString(AccessFlag.getAccessFlags(md.accessModifier)))
      methodTemplate.add("retTyp", md.returnType.typ.simpleName)
      methodTemplate.add("methodName", md.name)
      addImport(md.returnType.typ, imports)
    }

    val paramTemplates: Array[ST] = md.paramlist.map{
      param =>
        val paramTemplate = visitParamDeclaration(param, imports)
        paramTemplate
    }.toArray
    methodTemplate.add("params", paramTemplates)

    md.body match {
      case resolvedBody: ResolvedBody =>
        val localVars: Array[ST] = resolvedBody.locals.map {
          lv =>
            visitLocalVarDeclaration(lv, imports)
        }.toArray

        methodTemplate.add("localVars", localVars )

        val thisParam: Option[Param] = md.thisParam
//        val locationIter = resolvedBody.locations.toIterator
        val locationIter = LocationIterator(resolvedBody.locations)
        val currentState = CurrentState(isConstructor = isConstructor, isIfStatement = false, isElseIfStatement = false, isElseStatement = false, null)

        if (locationIter.hasNext) visitLocation(imports, bodyStatements, isConstructor, thisParam, locationIter, currentState)

      case UnresolvedBody(bodytokens) =>
    }

    val bodyTemplate = template.getInstanceOf("Body")
    bodyStatements.sortBy(_._1).map {
      st =>
        bodyTemplate.add("statements", st._2)
    }
    methodTemplate.add("body", bodyTemplate)
    println ("Body Statements are: " + bodyStatements)

    methodTemplate
  }

  //  private def visitLocation(imports: MSet[JawaType], bodyStatements: MList[(Int, ST)], isConstructor: Boolean, thisParam: Option[Param], loc: Location): Any = {
//  private def visitLocation(imports: MSet[JawaType], bodyStatements: MList[(Int, ST)], isConstructor: Boolean, thisParam: Option[Param], locationIter: Iterator[Location], currentState: CurrentState): Any = {
  private def visitLocation(imports: MSet[JawaType], bodyStatements: MList[(Int, ST)], isConstructor: Boolean, thisParam: Option[Param], locationIter: LocationIterator, currentState: CurrentState): Any = {
    val loc: Location = locationIter.next()
    println ("Location Symbol is: " + loc.locationSymbol)
    println ("Location Statement is: " + loc.statement)

    val statement: Statement = loc.statement
    //    visitStatement(imports, bodyStatements, isConstructor, thisParam, locationIter, loc, statement)
    visitStatement(imports, bodyStatements, isConstructor, thisParam, locationIter, loc, statement, currentState)

    if(locationIter.hasNext){
      //      visitLocation(imports, bodyStatements, isConstructor, thisParam, locationIter)
      visitLocation(imports, bodyStatements, isConstructor, thisParam, locationIter, currentState)
    }
  }

  private def visitStatement(imports: MSet[JawaType],
                             bodyStatements: MList[(Int, ST)],
                             isConstructor: Boolean,
                             thisParam: Option[Param],
//                             locationIter: Iterator[Location],
                             locationIter: LocationIterator,
                             loc: Location,
                             statement: Statement,
                             currentState: CurrentState): Any = {
    println ("current location is : " + loc.locationIndex)
    statement match {
      case as: AssignmentStatement =>
        bodyStatements += ((loc.locationIndex, visitAssignmentStatement(as, thisParam, imports)))

      case rs: ReturnStatement =>
        rs.varOpt match {
          case Some(v) =>
            val returnTemplate = template.getInstanceOf("ReturnStatement")
            println("Return Statement is: " + v.varName)
            returnTemplate.add("varName", v.varName)

            bodyStatements += ((loc.locationIndex, returnTemplate))
          case None =>
        }

      case cs: CallStatement =>
        if (!(cs.methodNameSymbol.methodName equals "<init>")) {
          bodyStatements += ((loc.locationIndex, visitCallStatement(cs, imports)))
        } else {
          println(" THis is Constructor : " + cs.args)

          val constructorCall: ST = visitConstructorCall(cs, imports)

          if (cs.isSuper && isConstructor) {
            println("This is a super Constructor!!!")
            constructorCall.remove("func")
            constructorCall.add("func", "super")
            bodyStatements += ((loc.locationIndex, constructorCall))
          } else {
            val prevLine: Option[(Int, ST)] = bodyStatements.lastOption
            prevLine match {
              case Some(prev) =>
                val prevTemplate: ST = prev._2
                val newTemplate: ST = template.getInstanceOf("NewExpression")

                newTemplate.add("baseType", constructorCall.getAttribute("func"))
                newTemplate.add("params", constructorCall.getAttribute("params"))

                prevTemplate.remove("rhs")
                prevTemplate.add("rhs", newTemplate)
                bodyStatements(bodyStatements.length - 1) = (loc.locationIndex, prevTemplate)

              case None =>
            }
          }
        }

      case ifStatement: IfStatement =>
        visitIfStatement(imports, bodyStatements, isConstructor, thisParam, locationIter, loc, currentState, ifStatement)
      /* //-----
       if(currentState.isElseStatement) {
         val elseTemplate: ST = template.getInstanceOf("IfStatement")
         elseTemplate.add("token", "else")
//          elseTemplate.add("cond", visitBinaryExpression(ifStatement.cond))

         val elseCurrentState = CurrentState(isConstructor = isConstructor, isIfStatement = false, isElseIfStatement = true, isElseStatement = false, Some(ifStatement.targetLocation.location))

         val elseBodyStatements: MList[(Int, ST)] = mlistEmpty

         visitIfBodyLocation(imports, elseBodyStatements, isConstructor, thisParam, locationIter, elseCurrentState, bodyStatements)

         val elseBodyTemplate = template.getInstanceOf("Body")
         elseBodyStatements.sortBy(_._1).map {
           st =>
             elseBodyTemplate.add("statements", st._2)
         }
         elseTemplate.add("body", elseBodyTemplate)
         println ("Else Body Statements are: " + elseBodyStatements)
         println ("location index after returning from Else: " + loc.locationIndex)

         bodyStatements += ((loc.locationIndex + 1, elseTemplate))
       }*/

      case _ =>
        println("Location statement not identified: " + loc.statement.getClass)
    }
  }

//  def visitIfStatement(imports: MSet[JawaType], bodyStatements: MList[(Int, ST)], isConstructor: Boolean, thisParam: Option[Param], locationIter: Iterator[Location], loc: Location, currentState: CurrentState, ifStatement: IfStatement): Unit = {
  def visitIfStatement(imports: MSet[JawaType], bodyStatements: MList[(Int, ST)], isConstructor: Boolean, thisParam: Option[Param], locationIter: LocationIterator, loc: Location, currentState: CurrentState, ifStatement: IfStatement): Unit = {
    //        val currentState = CurrentState(isConstructor = isConstructor, isIfStatement = true, isElseIfStatement = false, isElseStatement = false, ifStatement.targetLocation.location)
    //        val currentState = CurrentState(isConstructor = isConstructor, isIfStatement = true, isElseIfStatement = false, isElseStatement = false, Some(ifStatement.targetLocation.location))
    if (!currentState.isIfStatement && !currentState.isElseIfStatement && !currentState.isElseStatement) {
      println ("inside Original If Statement")
      currentState.isIfStatement = true
      currentState.targetLocation = ifStatement.targetLocation.location
    } else {
      println(" Original if check failed. : " + currentState.isIfStatement)
      println(" Original if check failed. : " + currentState.isElseIfStatement)
      println(" Original if check failed. : " + currentState.isElseStatement)
    }

    //        currentState.isIfStatement = true
    println("If Statement: " + ifStatement.cond) //Binary Expression
    println("If Statement: " + ifStatement.targetLocation.location)
    println("location index before visiting if: " + loc.locationIndex)

    val ifTemplate: ST = template.getInstanceOf("IfStatement")
    if (currentState.isIfStatement) {
      ifTemplate.add("token", ifStatement.ifToken.text)
      ifTemplate.add("cond", visitBinaryExpression(ifStatement.cond))
    } else if (currentState.isElseIfStatement) {
      ifTemplate.add("token", "else if")
      ifTemplate.add("cond", visitBinaryExpression(ifStatement.cond))
    } else {
      ifTemplate.add("token", "else")
    }

    val ifBodyStatements: MList[(Int, ST)] = mlistEmpty

    visitIfBodyLocation(imports, ifBodyStatements, isConstructor, thisParam, locationIter, currentState, bodyStatements)

    val ifBodyTemplate = template.getInstanceOf("Body")
    ifBodyStatements.sortBy(_._1).map {
      st =>
        ifBodyTemplate.add("statements", st._2)
    }
    ifTemplate.add("body", ifBodyTemplate)
    println("Body Statements are: " + ifBodyStatements)
    println("location index after returning from if: " + loc.locationIndex)

    bodyStatements += ((loc.locationIndex, ifTemplate))

    if(currentState.isElseStatement) {
      println ("this is else statement. calling visitIfStatement again")
      visitIfStatement(imports, bodyStatements, isConstructor, thisParam, locationIter, loc, currentState, ifStatement)
    }
  }

  case class CurrentState(
                           isConstructor: Boolean,
                           var isIfStatement: Boolean,
                           var isElseIfStatement: Boolean,
                           var isElseStatement: Boolean,
                                                        var targetLocation: String)
//                           var targetLocation: Option[String])

  private def visitIfBodyLocation(imports: MSet[JawaType],
                                  ifBodyStatements: MList[(Int, ST)],
                                  isConstructor: Boolean,
                                  thisParam: Option[Param],
//                                  locationIter: Iterator[Location],
                                  locationIter: LocationIterator,
                                  currentState: CurrentState,
                                  bodyStatements: MList[(Int, ST)]
                                 ): Any = {
    val loc: Location = locationIter.next()

    println ("If Body Location Symbol is: " + loc.locationSymbol)
    println ("If Body Location Statement is: " + loc.statement)

    val statement: Statement = loc.statement

    /*if(loc.locationUri == currentState.targetLocation) {
      println ("In the target location..No else statement required." )
      currentState.isIfStatement= false
      currentState.isElseIfStatement = false
      currentState.isElseStatement = false
      return
    }*/

    println ("Here in If target location: ")
    statement match {
      case gs: GotoStatement =>
        println("This is goto statement within if body. This indicates end of if body")
        if(currentState.isIfStatement) {
          println("Beginning else block now.")
          //todo assign bodyStatements. Start new ifStatement Template with token else. Change current status to else statement.
          currentState.isIfStatement = false
          currentState.isElseStatement = true
          currentState.targetLocation = gs.targetLocation.location
          println ("new target location is: " + currentState.targetLocation)
        }

        return
      case rs: ReturnStatement =>
        println("This is return statement within if body. This indicates end of if body")
        visitStatement(imports, ifBodyStatements, isConstructor, thisParam, locationIter, loc, statement, currentState)
        return

      case _ =>
        println ("case wild card!!!!")
//        visitStatement(imports, ifBodyStatements, isConstructor, thisParam, locationIter, loc, statement)
        visitStatement(imports, ifBodyStatements, isConstructor, thisParam, locationIter, loc, statement, currentState)
    }


    //    visitStatement(imports, ifBodyStatements, isConstructor, thisParam, locationIter, loc, statement)
    println("current line is : "+ loc.locationUri)
    println("look ahead line is : "+ locationIter.lookahead().locationUri)
    println("current target line is : "+ currentState.targetLocation)
    if(locationIter.lookahead().locationUri == currentState.targetLocation) {
      println ("In the target location..No else statement required." )
      currentState.isIfStatement= false
      currentState.isElseIfStatement = false
      currentState.isElseStatement = false
      return
    }

    if(locationIter.hasNext){
      visitIfBodyLocation(imports, ifBodyStatements, isConstructor, thisParam, locationIter, currentState, bodyStatements)
    }
  }

  private def visitAssignmentStatement(as: AssignmentStatement, thisParam: Option[Param], imports: MSet[JawaType]): ST = {
    val assignmentTemplate = template.getInstanceOf("AssignmentStatement")
    val lhs: ST = visitExpressionLHS(as.lhs, thisParam, imports)
    val rhs: ST = visitExpressionRHS(as.rhs, thisParam, imports)

    assignmentTemplate.add("lhs", lhs)
    assignmentTemplate.add("rhs", rhs)

    assignmentTemplate
  }

  private def visitExpressionLHS(lhs: Expression with LHS, thisParam: Option[Param], imports: MSet[JawaType]): ST = {
    lhs match {
      case ne: NameExpression =>
        visitNameExpression(ne, imports)

      //todo only tested for RHS
      case ae: AccessExpression =>
        visitAccessExpression(ae, thisParam, imports)

      case ie: IndexingExpression =>
        visitIndexingExpression(ie)

      case _ => throw new Jawa2JavaTranslateException("No matching LHS expression on line: " + lhs.pos.line + ":" + lhs.pos.column )
    }
  }

  private def visitExpressionRHS(rhs: Expression with RHS, thisParam:Option[Param], imports: MSet[JawaType]): ST = {
    rhs match {
      case ne: NameExpression =>
        visitNameExpression(ne, imports)

      case newExp: NewExpression =>
        println ("in new rhs expressions" + newExp.typ.simpleName)
        val newTemplate: ST = template.getInstanceOf("NewExpression")
        if (newExp.dimentions > 0) {
          newTemplate.add("baseType", newExp.typ.simpleName.replace("[]", ""))
          val params: Array[ST] = newExp.typeFragmentsWithInit.flatMap { t =>
            t.varNames map {
              v =>
                val arrayTemplate = template.getInstanceOf("ArrayAccess")
                arrayTemplate.add("arrayLength", v)
                println ("INside ")
                arrayTemplate
            }
          }.toArray

          newTemplate.add("arrays", params)
        } else {
          newTemplate.add("baseType", newExp.typ.simpleName)
        }
        newTemplate

      case le: LiteralExpression =>
        visitLiteralExpression(le)

      case ae: AccessExpression =>
        visitAccessExpression(ae, thisParam, imports)

      case ie: IndexingExpression =>
        visitIndexingExpression(ie)

      case ce: CastExpression =>
        visitCastExpression(ce, imports)

      case be: BinaryExpression =>
        visitBinaryExpression(be)

      //todo verify with fengguo.
      case cmp: CmpExpression =>
        visitCmpExpression(cmp)

      case insof: InstanceofExpression =>
        visitInstanceofExpression(insof, imports)

      case _ =>
        println ("In RHS :" + rhs.getClass)
        println ("In RHS :" + rhs.getFields)
        throw new Jawa2JavaTranslateException("No matching RHS expression on line: " + rhs.pos.line + ":" + rhs.pos.column )
    }
  }

  private def visitNameExpression(ne: NameExpression, imports: MSet[JawaType]): ST = {
    ne.varSymbol match {
      case Left(varSymbol) =>
        println ("in name expression Not Static: " + ne.name)

        val nameTemplate = template.getInstanceOf("NameExpression")
        nameTemplate.add("name", ne.name)
        nameTemplate

      case Right(fieldNameSymbol) =>
        println ("in name expression Static: " + ne.name)

        val staticTemplate = template.getInstanceOf("StaticNameExpression")
        staticTemplate.add("baseTyp", fieldNameSymbol.baseType.simpleName)
        staticTemplate.add("name", fieldNameSymbol.fieldName)
        addImport(fieldNameSymbol.baseType, imports)

        staticTemplate
    }
  }

  private def visitLiteralExpression(le: LiteralExpression): ST = {
    val litToken = le.constant.text

    le.constant.tokenType match  {
      case STRING_LITERAL =>
        val literalTemplate = template.getInstanceOf("StringLiteral")
        literalTemplate.add("str", le.getString)
        literalTemplate

      //todo int vs Integer cases??? Does Java byte code separate int vs long, float vs double???
      case FLOATING_POINT_LITERAL =>
        val numTemplate = template.getInstanceOf("NumericalLiteral")
        val leVal = litToken match {
          case x if x.endsWith("F") => le.getString + "F"
          case x if x.endsWith("D") => le.getString + "D"
          case _ => litToken
        }
        println ("inside numerical literal Floating point")
        numTemplate.add("nm", leVal)
        numTemplate

      case INTEGER_LITERAL =>
        val numTemplate = template.getInstanceOf("NumericalLiteral")
        val leVal = litToken match {
          case x if x.endsWith("I") => le.getString
          case x if x.endsWith("L") => le.getString + "L"
          case _ => litToken
        }
        println ("inside numerical literal Integer.")
        numTemplate.add("nm", leVal)
        numTemplate

      case CHARACTER_LITERAL =>
        val charTemplate = template.getInstanceOf("CharLiteral")
        charTemplate.add("chr", le.getString)
        charTemplate

      case _ => throw new Jawa2JavaTranslateException("No matching Literal Expression: " + le.pos.line + ":" + le.pos.column )
    }
  }

  private def visitAccessExpression(ae: AccessExpression, thisParam: Option[Param], imports: MSet[JawaType]): ST = {
    val accessTemplate = template.getInstanceOf("StaticNameExpression")
    val baseType: String = thisParam match {
      case Some(p) => if (p.name == ae.base) "this" else ae.base
      case None => ae.base
    }
    accessTemplate.add("baseTyp", baseType)
    accessTemplate.add("name", ae.fieldName)
    println ("Access Expression : " + ae)
    println ("Access Expression Base : " + ae.varSymbol.id)
    println ("Access Expression Field Name: " + ae.fieldName)
    accessTemplate
  }

  private def visitIndexingExpression(ie: IndexingExpression): ST = {
    println ("IN indexing expression" + ie.base)
    val indexingTemplate = template.getInstanceOf("IndexingExpression")
    indexingTemplate.add("name", ie.base)
    val indices: Array[Any] = ie.indices.map {
      idx =>
        idx.index match {
          case Left(varSymbol) =>
            val arrayTemplate = template.getInstanceOf("ArrayAccess")
            arrayTemplate.add("arrayLength", varSymbol.varName)
            println ("Indexing suffix  "+ varSymbol.varName)
            arrayTemplate
          case Right(lit) =>
            val arrayTemplate = template.getInstanceOf("ArrayAccess")
            arrayTemplate.add("arrayLength", lit.text)
            println ("Indexing suffix  "+ lit.text)
            arrayTemplate
        }
    }.toArray

    indexingTemplate.add("indices", indices)
    indexingTemplate
  }

  private def visitCastExpression(ce: CastExpression, imports: MSet[JawaType]): ST = {
    val castTemplate: ST = template.getInstanceOf("CastExpression")
    castTemplate.add("type", ce.typ.typ.simpleName)
    castTemplate.add("varName", ce.varName)
    addImport(ce.typ.baseType, imports)

    castTemplate
  }

  private def visitBinaryExpression(be: BinaryExpression): ST = {
    val binaryTemplate: ST = template.getInstanceOf("BinaryExpression")
    binaryTemplate.add("left", be.left.varName)
    binaryTemplate.add("op", be.op.text)
    val right: String = be.right match {
      case Left(varSym) => varSym.varName
      case Right(lit) => lit.text
    }
    binaryTemplate.add("right", right)
    binaryTemplate
  }

  private def visitInstanceofExpression(insof: InstanceofExpression, imports: MSet[JawaType]): ST = {
    println ("This is instance of expression: ")
    println ("This is instance of expression: " + insof.typExp.typ.simpleName + " instance " + insof.varSymbol.varName)

    val insofTemplate = template.getInstanceOf("InstanceofExpression")
    insofTemplate.add("var", insof.varSymbol.varName)
    insofTemplate.add("type", insof.typExp.typ.simpleName)
    addImport(insof.typExp.typ, imports)

    insofTemplate
  }

  private def visitCmpExpression(cmp: CmpExpression): ST = {
    val cmpTemplate: ST = template.getInstanceOf("BinaryExpression")
    println("In Cmp Expression var1: " + cmp.var1Symbol.varName)
    println("In Cmp Expression: " + cmp.cmp.text)
    println("In Cmp Expression var2: " + cmp.var2Symbol.varName)
    val op: String = cmp.cmp.text match {
      case "fcmpl" | "fcmpg" | "dcmpl" | "dcmpg" | "lcmp" => "<"
      case _ => throw new Jawa2JavaTranslateException("Unidentified cmp expression." + cmp.pos.line + ":" + cmp.pos.column )
    }
    cmpTemplate.add("left", cmp.var1Symbol.varName)
    cmpTemplate.add("op", op)
    cmpTemplate.add("right", cmp.var2Symbol.varName)
    cmpTemplate
  }

  private def visitConstructorCall(cc: CallStatement, imports: MSet[JawaType]): ST = {
    val callTemplate = template.getInstanceOf("CallStatement")
    val paramsTemplate = template.getInstanceOf("Params")
    val baseType: JawaType = cc.signature.getClassType
    callTemplate.add("func", baseType.simpleName)
    callTemplate.add("params", paramsTemplate.add("params", cc.args.toArray))
    addImport(baseType, imports)

    println ("Visit Constructor Call function name: " + baseType.simpleName)
    println ("Visit Constructor Call args: " + cc.args)

    callTemplate
  }


  private def visitCallStatement(cs: CallStatement, imports: MSet[JawaType]): ST = {
    val callTemplate = template.getInstanceOf("CallStatement")
    val paramsTemplate = template.getInstanceOf("Params")
    val baseType: JawaType = cs.signature.getClassType

    if(cs.isStatic) {
      val staticTemplate = template.getInstanceOf("StaticNameExpression")

      staticTemplate.add("baseTyp", baseType.simpleName)
      staticTemplate.add("name", cs.methodNameSymbol.methodName)
      callTemplate.add("func", staticTemplate)
    } else {
      callTemplate.add("func", cs.methodNameSymbol.methodName)
    }

    callTemplate.add("params", paramsTemplate.add("params", cs.args.toArray))
    addImport(baseType, imports)

    cs.lhsOpt match {
      case Some(lhs) =>
        val assignmentTemplate = template.getInstanceOf("AssignmentStatement")
        assignmentTemplate.add("lhs", lhs.lhs.varName)
        callTemplate.add("isAssignment", true)
        assignmentTemplate.add("rhs", callTemplate)
        assignmentTemplate
      case None =>
        callTemplate
    }
  }

  def visitLocalVarDeclaration(lvd: LocalVarDeclaration, imports: MSet[JawaType] ): ST = {
    val fieldTemplate = template.getInstanceOf("FieldDecl")

    fieldTemplate.add("accessFlag", AccessFlag.toString(AccessFlag.getAccessFlags(lvd.accessModifier)))
    fieldTemplate.add("attrTyp", lvd.typ.simpleName)
    fieldTemplate.add("attrName", lvd.varSymbol.varName)

    addImport(lvd.typ, imports)
    fieldTemplate
  }

  def visitParamDeclaration(param: Param, imports: MSet[JawaType] ): ST = {
    val paramTemplate = template.getInstanceOf("Param")

    paramTemplate.add("paramTyp", param.typ.typ.simpleName)
    paramTemplate.add("paramName", param.name)
    addImport(param.typ.typ, imports)

    paramTemplate
  }

  def addImport( jwt: JawaType, imports: MSet[JawaType]) = {
    jwt.getPackage match {
      case Some (pkg) =>
        imports += jwt
      case None =>
    }
  }
}
