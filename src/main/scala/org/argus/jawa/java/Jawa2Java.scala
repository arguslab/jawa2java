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
import org.argus.jawa.compiler.parser._
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


    if(md.isConstructor) {
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
        resolvedBody.locations foreach {
          loc =>
            println ("Location Symbol is: " + loc.locationSymbol)
            println ("Location Statement is: " + loc.statement)

            loc.statement match {
              case as: AssignmentStatement =>
                bodyStatements += ((loc.locationIndex, visitAssignmentStatement(as, thisParam, imports)))

              case rs: ReturnStatement =>
                rs.varOpt match {
                  case Some(v) =>
                    val returnTemplate = template.getInstanceOf("ReturnStatement")
                    println ("Return Statement is: " + v.varName)
                    returnTemplate.add("varName", v.varName)

                    bodyStatements += ((loc.locationIndex, returnTemplate))
                  case None =>
                }

              case cs: CallStatement =>
                if(!(cs.methodNameSymbol.methodName equals  "<init>" )){
                  bodyStatements += ((loc.locationIndex, visitCallStatement(cs, imports)))
                } else {
                  println (" THis is Constructor : " + cs.args)

                  if(cs.args.nonEmpty) {
                    val constructorCall: ST = visitConstructorCall(cs, imports)

                    if(cs.isSuper) {
                      println ("This is a super Constructor!!!")
                      constructorCall.remove("func")
                      constructorCall.add("func", "super")
                      bodyStatements += ((loc.locationIndex, constructorCall))
                    } else {
                      println("Body Last: "  + bodyStatements.lastOption)
                      val prevLine: Option[(Int, ST)] = bodyStatements.lastOption
                      prevLine match {
                        case Some(prev) =>
                          val prevTemplate = prev._2
                          constructorCall.add("isAssignment", true)
                          prevTemplate.remove("rhs")
                          prevTemplate.add("rhs", constructorCall)
                          bodyStatements(bodyStatements.length - 1) = (loc.locationIndex, prevTemplate)
                        //                  bodyStatements += ((loc.locationIndex, constructorCall))

                        case None =>
                      }
                    }
                  }
                }

              case _ =>
                println ("Location statement not identified: " + loc.statement.getClass)
            }
        }
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
      case FLOATING_POINT_LITERAL | INTEGER_LITERAL=>
        val numTemplate = template.getInstanceOf("NumericalLiteral")

        println ("inside numerical literal")
        numTemplate.add("nm", le.getString)
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

  private def visitConstructorCall(cc: CallStatement, imports: MSet[JawaType]): ST = {
    val callTemplate = template.getInstanceOf("CallStatement")
    val baseType: JawaType = cc.signature.getClassType
    callTemplate.add("func", baseType.simpleName)
    callTemplate.add("params", cc.args.toArray)
    addImport(baseType, imports)

    println ("Visit Constructor Call function name: " + baseType.simpleName)
    println ("Visit Constructor Call args: " + cc.args)

    callTemplate
  }


  private def visitCallStatement(cs: CallStatement, imports: MSet[JawaType]): ST = {
    val callTemplate = template.getInstanceOf("CallStatement")
    val baseType: JawaType = cs.signature.getClassType

    if(cs.isStatic) {
      val staticTemplate = template.getInstanceOf("StaticNameExpression")

      staticTemplate.add("baseTyp", baseType.simpleName)
      staticTemplate.add("name", cs.methodNameSymbol.methodName)
      callTemplate.add("func", staticTemplate)
    } else {
      callTemplate.add("func", cs.methodNameSymbol.methodName)
    }

    callTemplate.add("params", cs.args.toArray)
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
