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
   /* val locations = eitherLoc match {
      case Left(il) => il
      case Right(rl) => rl
    }*/

     var locations: List[Location] = eitherLoc match {
      case Left(il) => il
      case Right(rl) => rl.toList
    }
    var pos = 0
    //    val visitedLocations: MMap[Int, Int] = mmapEmpty

    def next(): Location = {
      val current: Location = locations(pos)
      pos += 1
      current
    }

    def hasNext: Boolean = {
      pos < locations.length
    }

    /*def lookahead(): Location = {
      locations(pos)
    }*/

    def setPos(locationIndex: Int): Unit = {
      pos = locations.indexWhere(l=> l.locationIndex == locationIndex)
    }

    /*def retrieveLocation(locationIndex: Int): Option[Location] = {
      val loc = locations.find(l => l.locationIndex == locationIndex)
      loc
    }*/

    /*def visitLocation(locationIndex: Int): Unit = {
      visitedLocations(locationIndex) = visitedLocations.getOrElse(locationIndex, 0) + 1
    }

    def getVisitedCount(locationIndex: Int): Int = {
      visitedLocations.getOrElse(locationIndex, 0)
    }


    def resetVisitedLocation(locationIndex: Int): Unit = {
      visitedLocations(locationIndex) = 0
    }*/


  }

  case class CurrentState(
                           isConstructor: Boolean,
                           var isIfStatement: Boolean,
                           var isElseIfStatement: Boolean,
                           var isElseStatement: Boolean,
                           var isLoop: Boolean = false,
                           var targetLocation: String,
                           var locationOffset: Int = 0,
                           parentState: Option[CurrentState] = None) {
    val visitedLocations: MMap[Int, Int] = mmapEmpty

    var nextLocation: Location = _

    def visitLocation(locationIndex: Int): Unit = {
      visitedLocations(locationIndex) = visitedLocations.getOrElse(locationIndex, 0) + 1
    }

    def getVisitedCount(locationIndex: Int): Int = {
      visitedLocations.getOrElse(locationIndex, 0)
    }

    def resetVisitedLocation(locationIndex: Int): Unit = {
      visitedLocations(locationIndex) = 0
    }

    //    var loopState: LoopState = _
    var loopState: AdditionalState = _

    var addToParent: MList[Location] = mlistEmpty
  }

  trait AdditionalState

  //  case class LoopState(ifLocation: Location, loopBackLocation: Location)
  case class LoopState(ifLocation: Int) extends AdditionalState
  case class IfState(ifLocation: Int) extends AdditionalState

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

    println("CURRENT METHOD IS: " + md.name)
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
        val currentState = CurrentState(isConstructor = isConstructor, isIfStatement = false, isElseIfStatement = false, isElseStatement = false, targetLocation = null)

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
                             locationIter: LocationIterator,
                             loc: Location,
                             statement: Statement,
                             currentState: CurrentState,
                             mainIter: LocationIterator): Unit = {
    println ("current location is : " + loc.locationIndex + " :: " + loc.locationSymbol.location)
    if(currentState.getVisitedCount(loc.locationIndex) > 0) {
      println ("this location has already been visited: " + loc.locationIndex + " :: " + loc.locationUri)
      return
    }

    /*val prevEntry = bodyStatements.find(b => b._1 == loc.locationIndex)

//    println("prev Entry is : " + prevEntry)
    prevEntry match {
      case Some(e) =>
        println("IF BLOCK REPLACEMENT!!!!!\n\n\n\n")
        bodyStatements += e
        return
      case None =>
    }*/

    statement match {
      case as: AssignmentStatement =>
        bodyStatements += ((loc.locationIndex, visitAssignmentStatement(as, thisParam, imports)))

      case rs: ReturnStatement =>
        rs.varOpt match {
          case Some(v) =>
            val returnTemplate = template.getInstanceOf("ReturnStatement")
            returnTemplate.add("varName", v.varName)

            if(!currentState.isIfStatement && !currentState.isElseIfStatement && !currentState.isElseStatement) {
              println ("This is the end of the program. No need to parse others. Setting iterator to end" )
              locationIter.setPos(locationIter.locations.length - 1)
            }

            bodyStatements += ((loc.locationIndex, returnTemplate))
          case None =>
        }

      case cs: CallStatement =>
        if (!(cs.methodNameSymbol.methodName equals "<init>")) {
          bodyStatements += ((loc.locationIndex, visitCallStatement(cs, imports)))
        } else {
          val constructorCall: ST = visitConstructorCall(cs, imports)

          if (cs.isSuper && isConstructor) {
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
        val ifCurrentState = CurrentState(isConstructor = isConstructor, isIfStatement = false, isElseIfStatement = false, isElseStatement = false, targetLocation = null, parentState = Some(currentState))

        val (ifBodyLocations: MList[Location], elseBodyLocations: MList[Location]) = prepareIfBodyStatements(imports, bodyStatements, isConstructor, thisParam, mainIter, loc, ifCurrentState, ifStatement)

        //Resetting addToParent locations to allow code reuse.
        ifCurrentState.parentState match {
          case Some(p) =>
            println("HEREREREREEEEEEEEE")
            p.addToParent.foreach{
              l =>
                println("RESETTING ADD TO PARENT: " + l.locationIndex + "  ::  " + l.locationUri)
                p.resetVisitedLocation(l.locationIndex)
            }
          case None =>
            throw new Jawa2JavaTranslateException("No Parent Block Found for this If Block. Location: " + loc.locationIndex + " :: " + loc.locationUri)
        }

        if(ifCurrentState.isLoop) {
          val elseLocationIter = LocationIterator(Right(elseBodyLocations))
          visitLoopStatement(imports, bodyStatements, isConstructor, thisParam, elseLocationIter, loc, ifCurrentState, ifStatement, mainIter, "loop")
        } else {
          val ifLocationIter = LocationIterator(Right(ifBodyLocations))
          visitIfStatement(imports, bodyStatements, isConstructor, thisParam, ifLocationIter, loc, ifCurrentState, ifStatement, mainIter, "if")

          val elseLocationIter = LocationIterator(Right(elseBodyLocations))
          visitIfStatement(imports, bodyStatements, isConstructor, thisParam, elseLocationIter, loc, ifCurrentState, ifStatement, mainIter, "else")
        }

        ifCurrentState.parentState match {
          case Some(p) => p.visitedLocations ++= ifCurrentState.visitedLocations
          case None =>
            throw new Jawa2JavaTranslateException("No Parent Block Found for this If Block. Location: " + loc.locationIndex + " :: " + loc.locationUri)
        }

        //todo pick one
        if(elseBodyLocations.nonEmpty) {
//          mainIter.setPos(elseBodyLocations.last.locationIndex + 1)
          locationIter.setPos(elseBodyLocations.last.locationIndex + 1)
          currentState.nextLocation = elseBodyLocations.last
        }

      case _ =>
    }
    //todo pick one??
    //    mainIter.visitLocation(loc.locationIndex)
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

    if(currentLocation < originalLocation) {
      println("IF WITH BACKWARD JUMP. HANDLE LATER!!!")

      // If Backward jump, first get the elseBodyStatements.
      // Set location iter to next location after current if statement. The mainIter is pointing to the previous if Statement.
      locationIter.setPos(originalLocation + 1)
      retrieveBackwardIfElseBodyLocations(elseBodyLocations, loc.locationIndex, ifStatement.targetLocation.locationIndex, locationIter, currentState)

    } else {

      // Set location iterator to the target location of If Jump. This iterator will now be used to follow the If statements.
      locationIter.setPos(currentLocation)

      retrieveIfBodyLocations(ifBodyLocations, elseBodyLocations, loc.locationIndex, ifStatement.targetLocation.locationIndex, locationIter, currentState)
    }
    //Debug
    println("If body Locations")
    ifBodyLocations.foreach(l => println(l.locationUri))
    println("Else body Locations")
    elseBodyLocations.foreach(l => println(l.locationUri))

    /*println("if body size: " + ifBodyLocations.size)
    for (elem <- elseBodyLocations) {
      if(ifBodyLocations.size == 0 && elem.statement.isInstanceOf[IfStatement] && elem.locationIndex == loc.locationIndex ) {
        println("this is a loop")
        currentState.isLoop = true
      }
    }*/

    // Reset the location iterator
    locationIter.setPos(originalLocation + 1)

    (ifBodyLocations, elseBodyLocations)
  }

  def retrieveBackwardIfElseBodyLocations(elseBodyLocations: MList[Location],
                                          startLocation: Int,
                                          originalTargetLocation: Int,
                                          locationIter: LocationIterator,
                                          currentState: CurrentState): Unit = {
    val loc = locationIter.next()
    println("In retrieve backward if else body locations: " + loc.locationUri + " :: " + loc.locationIndex)

    loc.statement match {
      case gs: GotoStatement =>
        val gotoLocation = gs.targetLocation.locationIndex

        currentState.parentState match {
          case Some(p) =>
            p.loopState match {
              case ls: LoopState =>
                if(gotoLocation > ls.ifLocation ){
                  println("Calling identify loop from NESTED within a LOOP backward if block.")
                  callIdentifyLoop(gs)
                } else {
                  //                  println("Adding location only from NESTED within a LOOP backward if block. ")
                  //                  addLocation(loc, elseBodyLocations )
                }

              case is: IfState =>
                println ("IFSTATE while retrieving Backward If Else Body Location. Implement Later!!!!")

              case _ => println("SHOULD NOT BE HERE!!!")
            }
          /*if(p.isLoop && gotoLocation > p.loopState.asInstanceOf[LoopState].ifLocation ){
            callIdentifyLoop(gs)
          } else {
            addLocation(loc, elseBodyLocations )
          }*/
          case None =>
            println("No Parent Case... Need to check for do...while loops.")
            addLocation(loc, elseBodyLocations )
            if(locationIter.hasNext) {
              retrieveBackwardIfElseBodyLocations(elseBodyLocations, startLocation, originalTargetLocation, locationIter, currentState)
            }
        }

      case _ =>
        addLocation(loc, elseBodyLocations )
        if(locationIter.hasNext) {
          retrieveBackwardIfElseBodyLocations(elseBodyLocations, startLocation, originalTargetLocation, locationIter, currentState)
        }
    }

    def addLocation(l: Location, loc: MList[Location]): Unit = {
      println("Adding location in Retrieve Backward If else Body Location..." + l.locationIndex + "  ::  " + l.locationUri)
      loc += l
    }

    def callIdentifyLoop(gs: GotoStatement): Unit = {
      println ("This Could be a loop. Check if it is a loop")
      val currentLocation = locationIter.pos
      locationIter.setPos(gs.targetLocation.locationIndex)
      identifyLoop(startLocation, locationIter, currentState, elseBodyLocations)
      locationIter.setPos(currentLocation)
    }
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
                              locationIter: LocationIterator,
                              currentState: CurrentState): Unit = {
    val loc = locationIter.next()


    loc.statement match {
      case gs: GotoStatement =>
        val gotoLocation = gs.targetLocation.locationIndex

        // This is a forward jump case. Add the location and follow the jump statement.
        if(gotoLocation > loc.locationIndex) {
          addLocation(loc, ifBodyLocations )
          retrieveIfBodyLocations(ifBodyLocations, elseBodyLocations, startLocation, originalTargetLocation, locationIter, currentState)
        }
        // Backward jump, but after the initial If location
        // Add the location and all locations from initial If statement (@startLocation) to the current goto target location(@gotoLocation)
        else if(gotoLocation < loc.locationIndex && gotoLocation > startLocation) {
          val addLocations = locationIter.locations.filter (l => l.locationIndex > startLocation && l.locationIndex < gotoLocation )
          addLocations.foreach(l =>println(l.locationUri + " :: " + l.locationIndex))
          elseBodyLocations ++= addLocations
        }
        // Backward jump, but jumps even before the initial If location
        // Add the location and all locations from initial If location(@startLocation) to the initial target location (originalTargetLocation)
        //This could be a loop
        else if (gotoLocation <= startLocation) {

          val addLocations = locationIter.locations.filter (l => l.locationIndex > startLocation && l.locationIndex < originalTargetLocation )
          addLocations.foreach(l =>println(l.locationUri))
//          removeLocationsFromIfBlock(addLocations)

          elseBodyLocations ++= addLocations
        }

      case rs: ReturnStatement =>
        val addLocations = locationIter.locations.filter (l => l.locationIndex > startLocation && l.locationIndex < originalTargetLocation )

        // Location limit can be due to a If statement as well as it also has a goto part.
//        val locationLimit: Option[Location] = addLocations find (al => al.statement.isInstanceOf[GotoStatement])

        removeLocationsFromIfBlock(addLocations)
        addLocations.foreach(l =>println("Adding to else block: " + l.locationUri))
        elseBodyLocations ++= addLocations

      case _ =>
        addLocation(loc, ifBodyLocations)
        retrieveIfBodyLocations(ifBodyLocations, elseBodyLocations, startLocation, originalTargetLocation, locationIter, currentState)
    }

    //    if(elseBodyLocations.nonEmpty && ifBodyLocations.isEmpty) {
    if(elseBodyLocations.nonEmpty ) {
      println("Checking else body for loop for if Statement at.: " + startLocation)
      elseBodyLocations.foreach(l => println("ELSE BODY locations are: " + l.locationUri + " :: " + l.locationIndex))
      ifBodyLocations.foreach(l => println("IF BODY locations are: " + l.locationUri + " :: " + l.locationIndex))
      //last statement may not be a goto. Find the last goto.

      val lastGoto = elseBodyLocations.lastIndexWhere(l => l.statement.isInstanceOf[GotoStatement])
      if (lastGoto != -1) {
        //          if(gs.targetLocation.locationIndex <= startLocation) {
        val gotoLocation: Location =  elseBodyLocations(lastGoto)
        val gs: GotoStatement = gotoLocation.statement.asInstanceOf[GotoStatement]
        println("Goto is at: " + gotoLocation.locationUri + " :: " + gotoLocation.locationIndex)

        val locationsToRemove = elseBodyLocations filter (l=> l.locationIndex > gotoLocation.locationIndex)
        for (r <- locationsToRemove) {
          elseBodyLocations.remove(elseBodyLocations.indexOf(r))
        }

        println("ADDING TO PARENT>>>>")

        val currentLocation = locationIter.pos
        locationIter.setPos(gs.targetLocation.locationIndex)
        identifyLoop(startLocation, locationIter, currentState, elseBodyLocations)

        //need to add removed statements to the parent block.
        currentState.parentState match {
          case Some(p) =>
            if(currentState.isLoop) {
              ifBodyLocations.foreach{
                l =>
                  println("ADDING IF BODY IN LOOP TO PARENT>>>" + l.locationIndex + " :: " + l.locationUri)
                  p.addToParent += l
              }
            }
            if(p.isLoop) {
              locationsToRemove.foreach{
                l =>
                  println("ADDING Removed IN LOOP TO PARENT>>>" + l.locationIndex + " :: " + l.locationUri)
                  p.addToParent += l
              }
            }
          case None =>
        }

        locationIter.setPos(currentLocation)
      } else {
        println("This is not a loop as else block has no goto statement.")
      }
    }

    def addLocation(l: Location, loc: MList[Location]): Unit = {
      loc += l
    }

    def removeLocationsFromIfBlock(addLocations: List[Location]): Unit = {
      val locationLimit: Option[Location] = addLocations find (al => al.statement.isInstanceOf[GotoStatement] || al.statement.isInstanceOf[IfStatement])
      // If there is a locationLimit -> Need to remove locations from @ifBodyLocations.
      locationLimit match {
        case Some(limit) =>
          addLocation(loc, ifBodyLocations)
          println("\n\n\n\n\n\n\n\nREMOVE LOCATION RETURN STATEMENT")

          val targetLocation: Int = limit.statement match {
            case gs: GotoStatement => gs.targetLocation.locationIndex
            case is: IfStatement => is.targetLocation.locationIndex
            case _ => -1
          }
          //            val locationsToRemove = ifBodyLocations filter (l=> l.locationIndex > limit.statement.asInstanceOf[GotoStatement].targetLocation.locationIndex)
          if (targetLocation != -1) {
            val locationsToRemove = ifBodyLocations filter (l => l.locationIndex >= targetLocation)
            for (r <- locationsToRemove) {
              println("Removing location from ifStatement Block: " + r.locationUri + " :: " + r.locationIndex)
              ifBodyLocations.remove(ifBodyLocations.indexOf(r))
            }
            ifBodyLocations.foreach(l => println("IfbodyLocations After removal are: " + l.locationIndex + " :: " + l.locationUri))
          }

        case None =>
      }
    }
  }

  /** Follow the goto to check if the original IfStatement
    * OR if the parent is a loop, then see if parents if can be reached or not. If it can be reached, then it is a loop.
    */
  def identifyLoop (startLocation: Int,
                    locationIter: LocationIterator,
                    currentState: CurrentState,
                    elseBodyLocations: MList[Location],
                    locationsToAdd: MList[Location] = mlistEmpty): Unit = {
    val loc = locationIter.next()
    //    val locationsToAdd: MList[Location] = mlistEmpty

    println("Identifying loop...")
    loc.statement match {
      case gs: GotoStatement =>
        //        println ("Not a loop : Goto")
        println("Checking Goto Statement. If it is not a backward jump, this is not a loop??? =>>> This is used in the Else Body evaluation Case. This evaluates the final goto statement.")
        if(gs.targetLocation.locationIndex < loc.locationIndex) {
          locationIter.setPos(gs.targetLocation.locationIndex)
          if(locationIter.hasNext) {
            identifyLoop(startLocation, locationIter, currentState, elseBodyLocations, locationsToAdd)
          }
        }
      case rs: ReturnStatement => println ("Not a loop : Return")

      case is: IfStatement =>
        println("Identifying loop if statement.")
        if(loc.locationIndex == startLocation){
          println ("This is a loop")
          currentState.isLoop = true

          //Setup Loop State:
          currentState.loopState = LoopState(startLocation)

          elseBodyLocations ++= locationsToAdd
          println("adding to elsebody" + loc.locationIndex + " :: " + loc.locationUri)
          elseBodyLocations.foreach(l => println(l.locationIndex + " :: " + l.locationUri))
        } else if (currentState.parentState.isDefined) {
          println("identify loop ifStatement else part.")
          //          locationIter.setPos(is.targetLocation.locationIndex)
          //          identifyLoop(startLocation, locationIter, currentState, elseBodyLocations, locationsToAdd)
          currentState.parentState match {
            case Some(p) =>
              if(p.isLoop && p.loopState.asInstanceOf[LoopState].ifLocation == startLocation){
                println ("This is a loop within a loop")
                currentState.isLoop = true
                elseBodyLocations ++= locationsToAdd
                println("adding to elsebody" + loc.locationIndex + " :: " + loc.locationUri)
                elseBodyLocations.foreach(l => println(l.locationIndex + " :: " + l.locationUri))
              } else if(p.isIfStatement || p.isElseIfStatement || p.isElseStatement){
                println ("PARENT IS IF STATEMENT>>>>>>>>")
              } else {  // parent is main. do the same as None case.
                if(is.targetLocation.locationIndex < startLocation) {
                  locationIter.setPos(is.targetLocation.locationIndex)
                  if(locationIter.hasNext) {
                    identifyLoop(startLocation, locationIter, currentState, elseBodyLocations, locationsToAdd)
                  }
                }
              }
            case None =>
              if(is.targetLocation.locationIndex < startLocation) {
                locationIter.setPos(is.targetLocation.locationIndex)
                if(locationIter.hasNext) {
                  identifyLoop(startLocation, locationIter, currentState, elseBodyLocations, locationsToAdd)
                }
              }
          }
        } else {
          if(is.targetLocation.locationIndex < startLocation) {
            locationIter.setPos(is.targetLocation.locationIndex)
            if(locationIter.hasNext) {
              identifyLoop(startLocation, locationIter, currentState, elseBodyLocations, locationsToAdd)
            }
          }
        }

      case _ =>
        if(!elseBodyLocations.contains(loc)) {
          println("adding to addlocations" + loc.locationIndex + " :: " + loc.locationUri)
          //          elseBodyLocations += loc
          locationsToAdd += loc

          //          elseBodyLocations.foreach(l => println(l.locationIndex + " :: " + l.locationUri))
          locationsToAdd.foreach(l => println(l.locationIndex + " :: " + l.locationUri))
          //Resetting visited.
          currentState.resetVisitedLocation(loc.locationIndex)
        }
        if(locationIter.hasNext) {
          identifyLoop(startLocation, locationIter, currentState, elseBodyLocations, locationsToAdd)
        }
    }
  }

  def visitLoopStatement(imports: MSet[JawaType],
                         bodyStatements: MList[(Int, ST)],
                         isConstructor: Boolean,
                         thisParam: Option[Param],
                         locationIter: LocationIterator,
                         loc: Location,
                         currentState: CurrentState,
                         ifStatement: IfStatement,
                         mainIter: LocationIterator,
                         key: String): Unit = {

    println(" \n\n\n\n\n\n\n START OF VISIT LOOP::::")
    locationIter.locations.foreach(l => println("locations to iterate." + l.locationIndex + " :: " + l.locationUri))
    currentState.visitedLocations.foreach{ case(k, v) => println("Visited locations." + k + " :: " + v)}
    val forPrint: Location = loc
    if (key == "loop") {
      println ("inside Loop Statement: " + loc.locationIndex + " :: " + loc.locationUri)

    } else {
      println ("inside Else Statement inside loop: SHOULD NOT BE HERE!" + loc.locationIndex + " :: " + loc.locationUri)
    }
    val loopTemplate: ST = template.getInstanceOf("WhileLoop")

    loopTemplate.add("cond", visitBinaryExpression(ifStatement.cond))

    val loopBodyStatements: MList[(Int, ST)] = mlistEmpty

    if(locationIter.hasNext){
      visitNewIfBodyLocation(imports, loopBodyStatements, isConstructor, thisParam, locationIter, currentState, bodyStatements, mainIter)
    }

    val loopBodyTemplate = template.getInstanceOf("Body")
    //    loopBodyStatements.sortBy(_._1).map { // removed sort to accomodate initialization variables. todo Find better way to track locations in main bodyStatements.
    loopBodyStatements map {
      st =>
        loopBodyTemplate.add("statements", st._2)
    }
    loopTemplate.add("body", loopBodyTemplate)


    //Resetting
    currentState.isLoop = false

    println("End of Visit LOOP Statement." + + forPrint.locationIndex + " :: " + forPrint.locationUri + "\n\n\n\n\n\n\n\n\n\n\n")
    bodyStatements += ((loc.locationIndex + currentState.locationOffset, loopTemplate))
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

    val ifTemplate: ST = template.getInstanceOf("IfStatement")
    //    if (currentState.isIfStatement) {
    if (key == "if") {
      ifTemplate.add("token", ifStatement.ifToken.text)
      ifTemplate.add("cond", visitBinaryExpression(ifStatement.cond))
    } else {
      ifTemplate.add("token", "else")
    }

    val ifBodyStatements: MList[(Int, ST)] = mlistEmpty

    if(locationIter.hasNext){
      visitNewIfBodyLocation(imports, ifBodyStatements, isConstructor, thisParam, locationIter, currentState, bodyStatements, mainIter)
    }

    val ifBodyTemplate = template.getInstanceOf("Body")
    ifBodyStatements.sortBy(_._1).map {
      st =>
        ifBodyTemplate.add("statements", st._2)
    }
    ifTemplate.add("body", ifBodyTemplate)

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
    val loc: Location =  locationIter.next()
    //    if(locationIter.getVisitedCount(loc.locationIndex) > 0) {
    if(currentState.getVisitedCount(loc.locationIndex) > 0) {
      println ("VISIT New If Body: this location has already been visited: " + loc.locationIndex)
      println ("%%%%%%%\n\n%%%%%%%%%")
    } else {
      val statement: Statement = loc.statement

      statement match {
        case gs: GotoStatement =>
          //          println("This is goto statement within if body. This indicates end of if body")
          if (currentState.isIfStatement) {
            //todo assign bodyStatements. Start new ifStatement Template with token else. Change current status to else statement.
            currentState.isIfStatement = false
            currentState.isElseStatement = true
            currentState.targetLocation = gs.targetLocation.location
            return
          } else if (currentState.isElseStatement) {
            currentState.isIfStatement = false
            currentState.isElseIfStatement = false
            currentState.isElseStatement = false
            return
          } else if (currentState.isLoop) {
            //            currentState.isLoop = false
            println("Goto in a loop. Look for initialisation statements added at the end. (Variable reuse case.)")
          }


        case rs: ReturnStatement =>
          //          println("This is return statement within if body. This indicates end of if body")
          visitStatement(imports, ifBodyStatements, isConstructor, thisParam, locationIter, loc, statement, currentState, mainIter)
          return

        case is: IfStatement =>
          val originalLocation = loc.locationIndex

          visitStatement(imports, ifBodyStatements, isConstructor, thisParam, mainIter, loc, statement, currentState, mainIter)

          //todo where to set this position??
          locationIter.setPos(originalLocation + 1)
          locationIter.locations.foreach(l => println("if iterator locations: " + l.locationIndex+ " :: " + l.locationUri))
        //          println("new Index pointed to: " + newCurrentState.nextLocation.locationIndex + " :: " + newCurrentState.nextLocation.locationUri)
        //          println("size of location iter: " + locationIter.locations.size )
        //          locationIter.setPos(locationIter.locations.indexOf(newCurrentState.nextLocation) + 1)

        case _ =>
          visitStatement(imports, ifBodyStatements, isConstructor, thisParam, locationIter, loc, statement, currentState, mainIter)
      }
    }

    if(locationIter.hasNext){
      //      visitNewIfBodyLocation(imports, ifBodyStatements, isConstructor, thisParam, locationIter, currentState, bodyStatements)
      visitNewIfBodyLocation(imports, ifBodyStatements, isConstructor, thisParam, locationIter, currentState, bodyStatements, mainIter)
    } else if(currentState.addToParent.nonEmpty){
      locationIter.locations ++= currentState.addToParent
      println("in add to parent walallalalal")
      currentState.addToParent = mlistEmpty
//      locationIter.locations.foreach(l=> currentState.resetVisitedLocation(l.locationIndex))
      locationIter.locations.foreach(l=> println ("in add to parent wala : " + l.locationUri + " :: " + l.locationIndex))
      visitNewIfBodyLocation(imports, ifBodyStatements, isConstructor, thisParam, locationIter, currentState, bodyStatements, mainIter)
    } else {
      //      println ("End of Statements in If Body..No else statement required?? Reset all flags..." )
      currentState.isIfStatement= false
      currentState.isElseIfStatement = false
      currentState.isElseStatement = false
      currentState.isLoop = false
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
        val newTemplate: ST = template.getInstanceOf("NewExpression")
        if (newExp.dimentions > 0) {
          newTemplate.add("baseType", newExp.typ.simpleName.replace("[]", ""))
          val params: Array[ST] = newExp.typeFragmentsWithInit.flatMap { t =>
            t.varNames map {
              v =>
                val arrayTemplate = template.getInstanceOf("ArrayAccess")
                arrayTemplate.add("arrayLength", v)
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
        throw new Jawa2JavaTranslateException("No matching RHS expression on line: " + rhs.pos.line + ":" + rhs.pos.column )
    }
  }

  private def visitNameExpression(ne: NameExpression, imports: MSet[JawaType]): ST = {
    ne.varSymbol match {
      case Left(varSymbol) =>

        val nameTemplate = template.getInstanceOf("NameExpression")
        nameTemplate.add("name", ne.name)
        nameTemplate

      case Right(fieldNameSymbol) =>

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
        numTemplate.add("nm", leVal)
        numTemplate

      case INTEGER_LITERAL =>
        val numTemplate = template.getInstanceOf("NumericalLiteral")
        val leVal = litToken match {
          case x if x.endsWith("I") => le.getString
          case x if x.endsWith("L") => le.getString + "L"
          case _ => litToken
        }
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
    accessTemplate
  }

  private def visitIndexingExpression(ie: IndexingExpression): ST = {
    val indexingTemplate = template.getInstanceOf("IndexingExpression")
    indexingTemplate.add("name", ie.base)
    val indices: Array[Any] = ie.indices.map {
      idx =>
        idx.index match {
          case Left(varSymbol) =>
            val arrayTemplate = template.getInstanceOf("ArrayAccess")
            arrayTemplate.add("arrayLength", varSymbol.varName)
            arrayTemplate
          case Right(lit) =>
            val arrayTemplate = template.getInstanceOf("ArrayAccess")
            arrayTemplate.add("arrayLength", lit.text)
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
      case Right(lit) =>

        //todo Find better handler for boolean
        if(be.left.varName.contains("boolean")) {
          if(lit.text == "0") {
            "false"
          } else {
            "true"
          }
        } else {
          lit.text
        }
    }
    binaryTemplate.add("right", right)
    binaryTemplate
  }

  private def visitInstanceofExpression(insof: InstanceofExpression, imports: MSet[JawaType]): ST = {

    val insofTemplate = template.getInstanceOf("InstanceofExpression")
    insofTemplate.add("var", insof.varSymbol.varName)
    insofTemplate.add("type", insof.typExp.typ.simpleName)
    addImport(insof.typExp.typ, imports)

    insofTemplate
  }

  private def visitCmpExpression(cmp: CmpExpression): ST = {
    val cmpTemplate: ST = template.getInstanceOf("BinaryExpression")
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
      cs.recvOpt match {
        case Some(s) =>
          val staticTemplate = template.getInstanceOf("StaticNameExpression")
          staticTemplate.add("baseTyp", s)
          staticTemplate.add("name", cs.methodNameSymbol.methodName)
          callTemplate.add("func", staticTemplate)
        case None =>
          callTemplate.add("func", cs.methodNameSymbol.methodName)
      }
    }

    println("Call Statement: " +cs.args)
    println("Call Statement: " +cs.recvOpt)
    cs.argVars.foreach(v => println(v.varName))

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
