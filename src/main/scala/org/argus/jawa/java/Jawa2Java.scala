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

  case class LocationIterator (eitherLoc: Either[IList[Location], MList[Location]]) {
    val locations = eitherLoc match {
      case Left(il) => il
      case Right(rl) => rl
    }
    var pos = 0
    val visitedLocations: MMap[Int, Int] = mmapEmpty
    val targetLocations: MMap[Int, Int] = mmapEmpty
    val nonReusableLocations: MList[Int] = mlistEmpty

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

    def visitLocation(locationIndex: Int): Unit = {
      visitedLocations(locationIndex) = visitedLocations.getOrElse(locationIndex, 0) + 1
    }

    def addTargetLocation(locationIndex: Int): Unit = {
      targetLocations(locationIndex) = targetLocations.getOrElse(locationIndex, 0) + 1
    }

    def addNonResuableLocation(locationIndex: Int): Unit = {
      nonReusableLocations += locationIndex
    }

    def getVisitedCount(locationIndex: Int): Int = {
      visitedLocations.getOrElse(locationIndex, 0)
    }

    def retrieveLocation(locationIndex: Int): Option[Location] = {
      val loc = locations.find(l => l.locationIndex == locationIndex)
      loc
    }

    def setPos(locationIndex: Int): Unit = {
      pos = locations.indexWhere(l=> l.locationIndex == locationIndex)
    }

    def resetVisitedLocation(locationIndex: Int): Unit = {
      visitedLocations(locationIndex) = 0
    }
  }

  case class CurrentState(
                           isConstructor: Boolean,
                           var isIfStatement: Boolean,
                           var isElseIfStatement: Boolean,
                           var isElseStatement: Boolean,
                           var targetLocation: String,
                           var locationOffset: Int = 0) {
    val visitedLocations: MMap[Int, Int] = mmapEmpty

    var nextLocation: Location = _

    def visitLocation(locationIndex: Int): Unit = {
      visitedLocations(locationIndex) = visitedLocations.getOrElse(locationIndex, 0) + 1
    }

    def getVisitedCount(locationIndex: Int): Int = {
      visitedLocations.getOrElse(locationIndex, 0)
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
//      println ("is Constructor Declaration: " + md.signature.getClassName)
//      println ("is Constructor Declaration: " + md.accessModifier)
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
        //        val locationIter = LocationIterator(resolvedBody.locations)
        val locationIter = LocationIterator(Left(resolvedBody.locations))
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
//    println ("Body Statements are: " + bodyStatements)

    methodTemplate
  }

  private def visitLocation(imports: MSet[JawaType], bodyStatements: MList[(Int, ST)], isConstructor: Boolean, thisParam: Option[Param], locationIter: LocationIterator, currentState: CurrentState): Unit = {
    val loc: Location = locationIter.next()
//    println ("Location Symbol is: " + loc.locationSymbol)
//    println ("Location Statement is: " + loc.statement)

    val statement: Statement = loc.statement
    visitStatement(imports, bodyStatements, isConstructor, thisParam, locationIter, loc, statement, currentState, locationIter)

    if(locationIter.hasNext){
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
                             currentState: CurrentState,
                             mainIter: LocationIterator): Any = {
    println ("current location is : " + loc.locationIndex + " :: " + loc.locationSymbol.location)
//    if(locationIter.getVisitedCount(loc.locationIndex) > 0) {
    if(currentState.getVisitedCount(loc.locationIndex) > 0) {
//    if(mainIter.getVisitedCount(loc.locationIndex) > 0) {
      println ("this location has already been visited: " + loc.locationIndex + " :: " + loc.locationUri)
      return
    } else {
//      println ("visited locations: " + locationIter.visitedLocations)
    }

    val prevEntry = bodyStatements.find(b => b._1 == loc.locationIndex)

//    println("prev Entry is : " + prevEntry)
    prevEntry match {
      case Some(e) =>
        bodyStatements += e
        return
      case None =>
    }

    statement match {
      case as: AssignmentStatement =>
        bodyStatements += ((loc.locationIndex, visitAssignmentStatement(as, thisParam, imports)))

      case rs: ReturnStatement =>
        rs.varOpt match {
          case Some(v) =>
            val returnTemplate = template.getInstanceOf("ReturnStatement")
//            println("Return Statement is: " + v.varName)
            returnTemplate.add("varName", v.varName)

            if(!currentState.isIfStatement && !currentState.isElseIfStatement && !currentState.isElseStatement) {
              println ("This is the end of the program. No need to parse others. Setting iterator to end" )
//              mainIter.setPos(mainIter.locations.length)
              locationIter.setPos(locationIter.locations.length - 1)
            }

            bodyStatements += ((loc.locationIndex, returnTemplate))
          case None =>
        }

      case cs: CallStatement =>
        if (!(cs.methodNameSymbol.methodName equals "<init>")) {
          bodyStatements += ((loc.locationIndex, visitCallStatement(cs, imports)))
        } else {
//          println(" THis is Constructor : " + cs.args)

          val constructorCall: ST = visitConstructorCall(cs, imports)

          if (cs.isSuper && isConstructor) {
//            println("This is a super Constructor!!!")
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
        val (ifBodyLocations: MList[Location], elseBodyLocations: MList[Location]) = prepareIfBodyStatements(imports, bodyStatements, isConstructor, thisParam, mainIter, loc, currentState, ifStatement)

        val ifLocationIter = LocationIterator(Right(ifBodyLocations))
        visitIfStatement(imports, bodyStatements, isConstructor, thisParam, ifLocationIter, loc, currentState, ifStatement, mainIter, "if")

        val elseLocationIter = LocationIterator(Right(elseBodyLocations))
        visitIfStatement(imports, bodyStatements, isConstructor, thisParam, elseLocationIter, loc, currentState, ifStatement, mainIter, "else")

        elseBodyLocations.foreach(l => mainIter.addNonResuableLocation(l.locationIndex))

//        println ("If Called from : " + loc.locationIndex + " :: " + loc.locationUri)
//        println ("If Called from : " + loc.locationIndex + " :: " + loc.locationUri)
//        println ("Resetting pos after if: " +  " new: " + (elseBodyLocations.last.locationIndex + 1 )+ " :: " + elseBodyLocations.last.locationUri)
        if(elseBodyLocations.nonEmpty) {
//          println ("Resetting to end of else from main visit.")
          mainIter.setPos(elseBodyLocations.last.locationIndex + 1)
          currentState.nextLocation = elseBodyLocations.last
        }

      case _ =>
//        println("Location statement not identified: " + loc.statement.getClass)
    }
    //    locationIter.visitLocation(loc.locationIndex)
    //Changed this to mainIter.
    mainIter.visitLocation(loc.locationIndex)
    currentState.visitLocation(loc.locationIndex)
  }

  def prepareIfBodyStatements(imports: MSet[JawaType],
                              bodyStatements: MList[(Int, ST)],
                              isConstructor: Boolean, thisParam: Option[Param],
                              locationIter: LocationIterator,
                              loc: Location,
                              currentState: CurrentState,
                              ifStatement: IfStatement): (MList[Location], MList[Location]) = {
    val ifBodyLocations: MList[Location] = mlistEmpty
    val elseBodyLocations: MList[Location] = mlistEmpty
    val originalLocation = loc.locationIndex
    val currentLocation = ifStatement.targetLocation.locationIndex

    // Set location iterator to the target location of If Jump. This iterator will now be used to follow the If statements.
    locationIter.setPos(currentLocation)

//    println ("Flag check before Retreive function check: " + currentState.isIfStatement + ": " + currentState.isElseStatement + " : " + currentState.isElseIfStatement)
//    println ("Iterator check before Retreive function check: " + locationIter.locations.getClass)
//    println ("Retreive function check: " + locationIter.retrieveLocation(loc.locationIndex))
    retrieveIfBodyLocations(ifBodyLocations, elseBodyLocations, loc.locationIndex, ifStatement.targetLocation.locationIndex, locationIter)

    // Mark all ifBodyLocations visited.
    //Trying to move it somewhere else. => Using mainIter for marking all locations visited in visitStatement
    //    ifBodyLocations.foreach(l => locationIter.visitLocation(l.locationIndex))

    //Debug
//    println ("IF BODY LOcations")
    ifBodyLocations.foreach(l => println(l.locationUri))
//    println ("ELSE  BODY LOcations")
    elseBodyLocations.foreach(l => println(l.locationUri))

    // Reset the location iterator
    locationIter.setPos(originalLocation + 1)
    /*if(elseBodyLocations.nonEmpty) {
      println ("Resetting to end of else from prepareif body" + (elseBodyLocations.last.locationIndex + 1))
      println ("Resetting to end of else from prepareif body" + (originalLocation + 1))
      locationIter.setPos(elseBodyLocations.last.locationIndex + 1)
    } else {
      locationIter.setPos(originalLocation + 1)
    }*/
    (ifBodyLocations, elseBodyLocations)
  }

  /**
    * Iteratively follow the if jump until a backward jump OR a return statement OR the end of file is encountered.
    *
    * @param ifBodyLocations  Collect all if body statements in this mutable list
    * @param startLocation  Initial If jump location (To track where the backward jump is)
    * @param originalTargetLocation Initial If jump target location (To track where the backward jump is)
    * @param locationIter Location Iterator to go through the If Body statements.
    */
  def retrieveIfBodyLocations(ifBodyLocations: MList[Location],
                              elseBodyLocations: MList[Location],
                              startLocation: Int,
                              originalTargetLocation: Int,
                              locationIter: LocationIterator): Unit = {
//    println ( "Retrieving next location in IFBodyLocation: " + locationIter.locations.getClass)
    val loc = locationIter.next()

    loc.statement match {
      case gs: GotoStatement =>
        val gotoLocation = gs.targetLocation.locationIndex
        locationIter.addTargetLocation(gotoLocation)

        // This is a forward jump case. Add the location and follow the jump statement.
        if(gotoLocation > loc.locationIndex) {
//          println ("In IF: adding location: " + loc.locationUri)
          addLocation(loc, ifBodyLocations )
          retrieveIfBodyLocations(ifBodyLocations, elseBodyLocations, startLocation, originalTargetLocation, locationIter)
        }
        // Backward jump, but after the initial If location
        // Add the location and all locations from initial If statement (@startLocation) to the current goto target location(@gotoLocation)
        else if(gotoLocation < loc.locationIndex && gotoLocation > startLocation) {
//          println ("In ELSE IF 1: adding location: " + loc.locationIndex + " > " + startLocation + "&&" + loc.locationIndex +" < " + gotoLocation )
          val addLocations = locationIter.locations.filter (l => l.locationIndex > startLocation && l.locationIndex < gotoLocation )
          addLocations.foreach(l =>println(l.locationUri + " :: " + l.locationIndex))
          elseBodyLocations ++= addLocations
        }
        // Backward jump, but jumps even before the initial If location
        // Add the location and all locations from initial If location(@startLocation) to the initial target location (originalTargetLocation)
        else if (gotoLocation < startLocation) {
//          println ("In ELSE IF 1: adding location: " )
          val addLocations = locationIter.locations.filter (l => l.locationIndex > startLocation && l.locationIndex < originalTargetLocation )
          addLocations.foreach(l =>println(l.locationUri))

          elseBodyLocations ++= addLocations
        }

      case rs: ReturnStatement =>
//        println ("case Return Statement in retrieveIfBodyLocations. Indicates end of method. ")
        val addLocations = locationIter.locations.filter (l => l.locationIndex > startLocation && l.locationIndex < originalTargetLocation )
        val locationLimit: Option[Location] = addLocations find (al => al.statement.isInstanceOf[GotoStatement])

        // If there is a locationLimit -> Need to remove locations from @ifBodyLocations.
        locationLimit match {
          case Some(limit) =>
            addLocation(loc, ifBodyLocations)
//            println ("Before Filtering:"+ ifBodyLocations.size)

            val locationsToRemove = ifBodyLocations filter (l=> l.locationIndex > limit.statement.asInstanceOf[GotoStatement].targetLocation.locationIndex)
//            println ("After Filtering: " + ifBodyLocations.size)
//            println ("After Filtering: " + locationsToRemove)
            for (r <- locationsToRemove) {
              ifBodyLocations.remove(ifBodyLocations.indexOf(r))
            }

          case None =>
//            println ("None in Return Statement in If. This Indicates that there is only else part in this if statement?? include the return statement outside if")
        }
        addLocations.foreach(l =>println("Adding: " + l.locationUri))
        elseBodyLocations ++= addLocations

      case _ =>
        addLocation(loc, ifBodyLocations)
        retrieveIfBodyLocations(ifBodyLocations, elseBodyLocations, startLocation, originalTargetLocation, locationIter)
    }

    def addLocation(l: Location, loc: MList[Location]): Unit = {
//      println("Adding location: " + l.locationUri + " :: " + l.locationIndex)
      loc += l
    }
  }

  def visitIfStatement(imports: MSet[JawaType],
                       bodyStatements: MList[(Int, ST)],
                       isConstructor: Boolean,
                       thisParam: Option[Param],
                       locationIter: LocationIterator,
                       loc: Location,
                       currentState: CurrentState,
                       ifStatement: IfStatement,
                       mainIter: LocationIterator,
                       key: String): Unit = {
//    if (!currentState.isIfStatement && !currentState.isElseIfStatement && !currentState.isElseStatement) {
    val forPrint: Location = loc
    if (key == "if") {
      println ("inside Original If Statement: " + loc.locationIndex + " :: " + loc.locationUri)
      currentState.isIfStatement = true
      currentState.targetLocation = ifStatement.targetLocation.location
    } else {
      println ("inside Else Statement: " + loc.locationIndex + " :: " + loc.locationUri)
    }

//    println("If Statement: " + ifStatement.cond) //Binary Expression
//    println("If Statement: " + ifStatement.targetLocation.location)
//    println("location index before visiting if: " + loc.locationIndex)

    val ifTemplate: ST = template.getInstanceOf("IfStatement")
//    if (currentState.isIfStatement) {
    if (key == "if") {
//      println ("THis isIfStatement: " + locationIter.locations)
//      println ("THis isIfStatement: " + locationIter.locations.size)
//      println ("THis isIfStatement: " + locationIter.pos)
      ifTemplate.add("token", ifStatement.ifToken.text)
      ifTemplate.add("cond", visitBinaryExpression(ifStatement.cond))
    } else {
//      println ("THis isElseStatement: " + locationIter.locations)
//      println ("THis isElseStatement: " + locationIter.locations.size)
//      println ("THis isElseStatement: " + locationIter.pos)
      ifTemplate.add("token", "else")
    }

    val ifBodyStatements: MList[(Int, ST)] = mlistEmpty
//    println ("SECOND CALL : " + locationIter.locations.getClass)
//    println ("SECOND CALL : " + locationIter.locations.size)

    if(locationIter.hasNext){
      visitNewIfBodyLocation(imports, ifBodyStatements, isConstructor, thisParam, locationIter, currentState, bodyStatements, mainIter)
    }

    val ifBodyTemplate = template.getInstanceOf("Body")
    ifBodyStatements.sortBy(_._1).map {
      st =>
        ifBodyTemplate.add("statements", st._2)
    }
    ifTemplate.add("body", ifBodyTemplate)
//    println("Body Statements are: " + ifBodyStatements)
//    println("location index after returning from if: " + loc.locationIndex)
//    println("IFBODY IS : " + ifTemplate.render())

    println("End of Visit If Statement." + + forPrint.locationIndex + " :: " + forPrint.locationUri)
    bodyStatements += ((loc.locationIndex + currentState.locationOffset, ifTemplate))
  }

  private def visitNewIfBodyLocation(imports: MSet[JawaType],
                                     ifBodyStatements: MList[(Int, ST)],
                                     isConstructor: Boolean,
                                     thisParam: Option[Param],
                                     locationIter: LocationIterator,
                                     currentState: CurrentState,
                                     bodyStatements: MList[(Int, ST)],
                                     mainIter: LocationIterator
                                    ): Unit = {
//    println("location Iter : " + locationIter.locations.getClass)
//    println("location Iter : " + locationIter.locations.size)
//    println("location Iter : " + locationIter.pos)
//    println("location Iter : " + locationIter.visitedLocations)
    val loc: Location =  locationIter.next()

//    println ("Location to visit next: " + loc.locationIndex + " :::: " + loc.locationSymbol.location)
//    println ("target locations: : " + mainIter.targetLocations)
//    if(locationIter.getVisitedCount(loc.locationIndex) > 0) {
//    if(locationIter.getVisitedCount(loc.locationIndex) > 0) {
    if(currentState.getVisitedCount(loc.locationIndex) > 0) {
      println ("VISIT New If Body: this location has already been visited: " + loc.locationIndex)
      println ("%%%%%%%\n\n%%%%%%%%%")
    } else {
      println("in else")
      val statement: Statement = loc.statement

//      println("Here in If target location: ")
      statement match {
        case gs: GotoStatement =>
          mainIter.addTargetLocation(gs.targetLocation.locationIndex)
//          println("This is goto statement within if body. This indicates end of if body")
          if (currentState.isIfStatement) {
//            println("Beginning else block now.")
            //todo assign bodyStatements. Start new ifStatement Template with token else. Change current status to else statement.
            currentState.isIfStatement = false
            currentState.isElseStatement = true
            currentState.targetLocation = gs.targetLocation.location
//            println("new target location is: " + currentState.targetLocation)
          } else if (currentState.isElseStatement) {
//            println("This is else statement. Resetting all If flags.")
            currentState.isIfStatement = false
            currentState.isElseIfStatement = false
            currentState.isElseStatement = false
          }
          return

        case rs: ReturnStatement =>
//          println("This is return statement within if body. This indicates end of if body")
          visitStatement(imports, ifBodyStatements, isConstructor, thisParam, locationIter, loc, statement, currentState, mainIter)
          return

        case is: IfStatement =>
//          println("This is Inner nestede if....." + mainIter.locations.getClass)
//          println("This is Inner nestede if....." + mainIter.locations.size)
//          println("This is Inner nestede if....." + mainIter.pos)
//          println("This is Inner nestede if....." + locationIter.locations.getClass)
//          println("This is Inner nestede if....." + locationIter.locations.size)
//          println("This is Inner nestede if....." + locationIter.pos + " \n\n\n\n\n\n\n")
          val originalLocation = loc.locationIndex

          // Resetting this location as all If Body statements are marked visited before it is visited.
          // Check if this is still required as marking visited has been moved.
          mainIter.resetVisitedLocation(originalLocation)
          val newCurrentState = CurrentState(isConstructor = isConstructor, isIfStatement = false, isElseIfStatement = false, isElseStatement = false, null)

          visitStatement(imports, ifBodyStatements, isConstructor, thisParam, mainIter, loc, statement, newCurrentState, mainIter)
          // This will change the ifBodyLocation iterator(@locationIter) => need to reset it?

          currentState.visitedLocations ++= newCurrentState.visitedLocations
          currentState.visitedLocations.foreach(l => println("parent visited: " + l._1))
          newCurrentState.visitedLocations.foreach(l => println("new visited: " + l._1))
//          println("RETURNED FROM NESTED IF::: ")
//
//          println("RETURNED FROM NESTED IF....." + mainIter.locations.getClass)
//          println("RETURNED FROM NESTED IF....." + mainIter.locations.size)
//          println("RETURNED FROM NESTED IF....." + mainIter.pos)
//          println("RETURNED FROM NESTED IF....." + locationIter.locations.getClass)
//          println("RETURNED FROM NESTED IF....." + locationIter.locations.size)
//          println("RETURNED FROM NESTED IF....." + locationIter.pos + " \n\n\n\n\n\n\n")
          //todo where to set this position??
          locationIter.setPos(originalLocation + 1)
          locationIter.locations.foreach(l => println("if iterator locations: " + l.locationIndex+ " :: " + l.locationUri))
          println("new Index pointed to: " + newCurrentState.nextLocation.locationIndex + " :: " + newCurrentState.nextLocation.locationUri)
          println("size of location iter: " + locationIter.locations.size )
//          locationIter.setPos(locationIter.locations.indexOf(newCurrentState.nextLocation) + 1)

        case _ =>
//          println("case wild card!!!!")
          visitStatement(imports, ifBodyStatements, isConstructor, thisParam, locationIter, loc, statement, currentState, mainIter)
      }
    }

    if(locationIter.hasNext){
      //      visitNewIfBodyLocation(imports, ifBodyStatements, isConstructor, thisParam, locationIter, currentState, bodyStatements)
      visitNewIfBodyLocation(imports, ifBodyStatements, isConstructor, thisParam, locationIter, currentState, bodyStatements, mainIter)
    } else {
//      println ("End of Statements in If Body..No else statement required?? Reset all flags..." )
      currentState.isIfStatement= false
      currentState.isElseIfStatement = false
      currentState.isElseStatement = false
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
//        println ("in new rhs expressions" + newExp.typ.simpleName)
        val newTemplate: ST = template.getInstanceOf("NewExpression")
        if (newExp.dimentions > 0) {
          newTemplate.add("baseType", newExp.typ.simpleName.replace("[]", ""))
          val params: Array[ST] = newExp.typeFragmentsWithInit.flatMap { t =>
            t.varNames map {
              v =>
                val arrayTemplate = template.getInstanceOf("ArrayAccess")
                arrayTemplate.add("arrayLength", v)
//                println ("INside ")
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
//        println ("In RHS :" + rhs.getClass)
//        println ("In RHS :" + rhs.getFields)
        throw new Jawa2JavaTranslateException("No matching RHS expression on line: " + rhs.pos.line + ":" + rhs.pos.column )
    }
  }

  private def visitNameExpression(ne: NameExpression, imports: MSet[JawaType]): ST = {
    ne.varSymbol match {
      case Left(varSymbol) =>
//        println ("in name expression Not Static: " + ne.name)

        val nameTemplate = template.getInstanceOf("NameExpression")
        nameTemplate.add("name", ne.name)
        nameTemplate

      case Right(fieldNameSymbol) =>
//        println ("in name expression Static: " + ne.name)

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
//        println ("inside numerical literal Floating point")
        numTemplate.add("nm", leVal)
        numTemplate

      case INTEGER_LITERAL =>
        val numTemplate = template.getInstanceOf("NumericalLiteral")
        val leVal = litToken match {
          case x if x.endsWith("I") => le.getString
          case x if x.endsWith("L") => le.getString + "L"
          case _ => litToken
        }
//        println ("inside numerical literal Integer.")
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
//    println ("Access Expression : " + ae)
//    println ("Access Expression Base : " + ae.varSymbol.id)
//    println ("Access Expression Field Name: " + ae.fieldName)
    accessTemplate
  }

  private def visitIndexingExpression(ie: IndexingExpression): ST = {
//    println ("IN indexing expression" + ie.base)
    val indexingTemplate = template.getInstanceOf("IndexingExpression")
    indexingTemplate.add("name", ie.base)
    val indices: Array[Any] = ie.indices.map {
      idx =>
        idx.index match {
          case Left(varSymbol) =>
            val arrayTemplate = template.getInstanceOf("ArrayAccess")
            arrayTemplate.add("arrayLength", varSymbol.varName)
//            println ("Indexing suffix  "+ varSymbol.varName)
            arrayTemplate
          case Right(lit) =>
            val arrayTemplate = template.getInstanceOf("ArrayAccess")
            arrayTemplate.add("arrayLength", lit.text)
//            println ("Indexing suffix  "+ lit.text)
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
//    println ("This is instance of expression: ")
//    println ("This is instance of expression: " + insof.typExp.typ.simpleName + " instance " + insof.varSymbol.varName)

    val insofTemplate = template.getInstanceOf("InstanceofExpression")
    insofTemplate.add("var", insof.varSymbol.varName)
    insofTemplate.add("type", insof.typExp.typ.simpleName)
    addImport(insof.typExp.typ, imports)

    insofTemplate
  }

  private def visitCmpExpression(cmp: CmpExpression): ST = {
    val cmpTemplate: ST = template.getInstanceOf("BinaryExpression")
//    println("In Cmp Expression var1: " + cmp.var1Symbol.varName)
//    println("In Cmp Expression: " + cmp.cmp.text)
//    println("In Cmp Expression var2: " + cmp.var2Symbol.varName)
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

//    println ("Visit Constructor Call function name: " + baseType.simpleName)
//    println ("Visit Constructor Call args: " + cc.args)

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
