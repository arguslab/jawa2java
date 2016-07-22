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
import org.argus.jawa.core.io.SourceFile
import org.argus.jawa.core.{AccessFlag, JawaType, Reporter}
import org.sireum.util._
import org.stringtemplate.v4.{ST, STGroupFile}
import scala.util.control.Breaks._

import scala.collection.mutable


/**
  * Translate Jawa to Java.
  *
  * @author <a href="mailto:fgwei521@gmail.com">Fengguo Wei</a>
  * @author <a href="mailto:anwesh.tuladhar@gmail.com">Anwesh Tuladhar</a>
  */
class Jawa2JavaNew(reporter: Reporter) {

  private val template = new STGroupFile("templates/JavaModel.stg")

  trait TaskHelper {
    @throws(classOf[Exception])
    //    def identifyTask(loc: Location, locationIterator: LocationIterator, mainTask: MainTask, currentTask: BlockTask, parentTask: Option[BlockTask] = None): Task = {
    def identifyTask(loc: Location, locationIterator: LocationIterator, mainTask: MainTask, currentTask: BlockTask, parentTask: BlockTask): Task = {
      val statement: Statement = loc.statement

      println("current location is ###: " + loc.locationIndex + " :: " + loc.locationSymbol.location)

      statement match {
        case ifStatement: IfStatement =>
          //          val ifCurrentState = CurrentState(isConstructor = mainTask.isConstructor, parentTask = parentTask)
          val ifCurrentState = CurrentState(isConstructor = mainTask.isConstructor, parentTask = Some(currentTask))
          /* parentTask match {
             case Some(p) =>
               p match {
                 case it: IfTask =>
                   ifCurrentState.isIfStatement = true
                 case wt: WhileTask =>
                   ifCurrentState.isLoop = true
                 case dwt: DoWhileTask =>
                   ifCurrentState.isDoWhile = true
                 case _ =>
               }
             case None =>
           }*/
          //WHy did I put this here???
          /*parentTask match {
                case it: IfTask =>
                  ifCurrentState.isIfStatement = true
                case wt: WhileTask =>
                  ifCurrentState.isLoop = true
                case dwt: DoWhileTask =>
                  ifCurrentState.isDoWhile = true
                case _ =>
              }*/

          val (ifBodyLocations: MList[Location], elseBodyLocations: MList[Location]) = prepareIfBodyStatements(mainTask.locationIterator, loc, ifCurrentState, ifStatement)
          println("THE END OF BLOCK at: "+ loc.locationIndex + " :: " + loc.locationUri+ " is ::::" + ifCurrentState.endOfLoop)
          if (ifCurrentState.isLoop) {
            println("This is a while loop block")
            val whileTask = WhileTask(loc, elseBodyLocations, mainTask, currentTask)
            //            whileTask.endOfBlock = ifCurrentState.endOfLoop.locationIndex
            whileTask.endOfBlock = ifCurrentState.endOfLoop

            //Debug
            //            whileTask.loopBodyLocations.foreach(l=> println("Identified While Block Tasks. " + l.locationUri + " :: " +l.locationUri))

            whileTask
          }
          else if (ifCurrentState.isDoWhile) {
            println("This is a do while loop block.")
            val doWhileTask = DoWhileTask(loc, elseBodyLocations, mainTask, currentTask)
            //            doWhileTask.loopBodyLocations.foreach(l=> println("Identified Do...While Block Tasks. "  + l.locationUri + " :: " +l.locationUri))
            doWhileTask.endOfBlock = ifCurrentState.endOfLoop

            doWhileTask
          }
          else {
            /* println("This is an if...else block")
             val ifTask = IfTask(loc, ifBodyLocations, elseBodyLocations, mainTask, currentTask)
             ifTask.ifBodyLocations.foreach(l=> println("Identified IF Block Tasks. "  + l.locationUri + " :: " +l.locationUri))
             ifTask.elseBodyLocations.foreach(l=> println("Identified Else Block Tasks. " + l.locationUri + " :: " +l.locationUri))
             ifTask.endOfBlock = elseBodyLocations.last.locationIndex + 1

             ifTask*/
            println("This is an if...else block")
            val ifElseTask = IfElseTask(loc, ifBodyLocations, elseBodyLocations, mainTask, currentTask)
            //            ifElseTask.ifBodyLocations.foreach(l=> println("Identified IF Block Tasks. "  + l.locationUri + " :: " +l.locationUri))
            //            ifElseTask.elseBodyLocations.foreach(l=> println("Identified Else Block Tasks. " + l.locationUri + " :: " +l.locationUri))
            //            ifElseTask.endOfBlock = elseBodyLocations.last.locationIndex + 1
            val endOfIf = if(ifBodyLocations.nonEmpty) ifBodyLocations.last.locationIndex else -1
            val endOfElse = elseBodyLocations.last.locationIndex

            println("PARENT OF IF ELSE IS: " + parentTask.getClass)
            //This logic fails in some cases: eg. nestedWhileConditional
            /*if(parentTask.checkLocation(endOfIf)) {
              ifElseTask.endOfBlock = if(endOfIf > endOfElse) endOfIf else endOfElse
              println("SETIING END OF IF " + ifElseTask.endOfBlock + "\n\n")
            } else {
              println("SETTING END OF IF TO END OF ELSE")
              ifElseTask.endOfBlock = endOfElse
            }*/

            ifElseTask.endOfBlock = elseBodyLocations.last.locationIndex

            ifElseTask
          }
        case _ =>
          SimpleTask(loc, mainTask)
      }
    }

    def prepareIfBodyStatements(locationIter: LocationIterator,
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
        retrieveBackwardIfElseBodyLocations1(elseBodyLocations, loc.locationIndex, ifStatement.targetLocation.locationIndex, locationIter, currentState)
        if(currentState.isDoWhile) {
          //          ifBodyLocations.foreach(l=> println("doloop if" + l.locationIndex + " :: " + l.locationUri))
          //          elseBodyLocations.foreach(l=> println("do loop else" + l.locationIndex + " :: " + l.locationUri))
        }
      } else {

        // Set location iterator to the target location of If Jump. This iterator will now be used to follow the If statements.
        locationIter.setPos(currentLocation)

        retrieveIfBodyLocations(ifBodyLocations, elseBodyLocations, loc.locationIndex, ifStatement.targetLocation.locationIndex, locationIter, currentState)
        println("AFTER RETRIEVE IF BODY LOCATIONS!!!!")
      }

      // Reset the location iterator
      locationIter.setPos(originalLocation + 1)
      println("\n\n$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\nIDENTIFY LOOP RESULT: If at : " + loc.locationIndex + " :: " +loc.locationUri + " is a loop :: " + currentState.isLoop +  " :dowhile: " + currentState.isDoWhile + "\n$$$$$$$$$$$$$$$$$\n\n")


      (ifBodyLocations, elseBodyLocations)
    }

    def checkDoWhileLoop(elseBodyLocations: MList[Location],
                         startLocation: Int,
                         originalTargetLocation: Int,
                         locationIter: LocationIterator,
                         currentState: CurrentState): Unit = {
      val locationsToAdd = locationIter.locations.filter (l => l.locationIndex >= originalTargetLocation && l.locationIndex <= startLocation )
      elseBodyLocations ++= locationsToAdd
      println("CHECKING ")
      println ("This Could be a loop. Check if it is a loop")
      val currentLocation = locationIter.pos

      //      elseBodyLocations.foreach(l=> println("DOWHILE: " + l.locationIndex + " :: " + l.locationUri))
      locationIter.setPos(elseBodyLocations.head.locationIndex)

      identifyDoLoop(startLocation, locationIter, currentState, elseBodyLocations)

      println("Finished checking for do while")

      if(currentState.isLoop) {
        currentState.isDoWhile = true
        currentState.isLoop = false
      }
      locationIter.setPos(currentLocation)
    }

    def retrieveBackwardIfElseBodyLocations1(elseBodyLocations: MList[Location],
                                             startLocation: Int,
                                             originalTargetLocation: Int,
                                             locationIter: LocationIterator,
                                             currentState: CurrentState): Unit = {
      val elseBody: MList[Location] = mlistEmpty

      checkDoWhileLoop(elseBody, startLocation, originalTargetLocation, locationIter, currentState)
      if (currentState.isDoWhile) {
        elseBodyLocations ++= elseBody
        println("THis is a do while loop")

      } else {
        println("Calling original retreive.")
        locationIter.setPos(startLocation +1 )
        retrieveBackwardIfElseBodyLocations(elseBodyLocations, startLocation, originalTargetLocation, locationIter, currentState)
        //        elseBodyLocations.foreach(l=> println("Backward Body: " + l.locationIndex + " :: " + l.locationUri))
      }
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
          println("GOTO IN BACKWARD")
          val gotoLocation = gs.targetLocation.locationIndex
          addLocation(loc, elseBodyLocations )
          currentState.endOfLoop = loc.locationIndex

          currentState.parentTask match {
            case Some(p) =>
              p  match {
                case wt: WhileTask =>
                  if(gotoLocation > wt.location.locationIndex ){
                    println("Calling identify loop from NESTED within a LOOP backward if block.")
                    callIdentifyLoop(gs)
                  } else {
                    println("Adding location only from NESTED within a LOOP backward if block. ")
                    //                  addLocation(loc, elseBodyLocations )
                  }

                case dwt: DoWhileTask =>
                  if(gotoLocation > dwt.location.locationIndex ){
                    println("Calling identify loop from NESTED within a LOOP backward if block.")
                    callIdentifyLoop(gs)
                  } else {
                    println("Adding location only from NESTED within a LOOP backward if block. ")
                    //                  addLocation(loc, elseBodyLocations )
                  }

                case it: IfTask =>
                  println ("IFSTATE while retrieving Backward If Else Body Location. Implement Later!!!!")

                case _ => println("SHOULD NOT BE HERE!!!")
              }

            case None =>
              println("No Parent Case... Need to check for do...while loops.")
              addLocation(loc, elseBodyLocations )
              if(locationIter.hasNext) {
                retrieveBackwardIfElseBodyLocations(elseBodyLocations, startLocation, originalTargetLocation, locationIter, currentState)
              }
          }

        /* currentState.parentState match {
           case Some(p) =>
             p.additionalState match {
               case ls: LoopState =>
                 if(gotoLocation > ls.ifLocation ){
                   println("Calling identify loop from NESTED within a LOOP backward if block.")
                   callIdentifyLoop(gs)
                 } else {
                   println("Adding location only from NESTED within a LOOP backward if block. ")
                   //                  addLocation(loc, elseBodyLocations )
                 }

               case is: IfState =>
                 println ("IFSTATE while retrieving Backward If Else Body Location. Implement Later!!!!")

               case _ => println("SHOULD NOT BE HERE!!!")
             }

           case None =>
             println("No Parent Case... Need to check for do...while loops.")
             addLocation(loc, elseBodyLocations )
             if(locationIter.hasNext) {
               retrieveBackwardIfElseBodyLocations(elseBodyLocations, startLocation, originalTargetLocation, locationIter, currentState)
             }
         }*/

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
        println ("This Could be a loop. Check if it is a loop from backward")
        val currentLocation = locationIter.pos
        //Changing loop identification to top down.
        //        locationIter.setPos(gs.targetLocation.locationIndex)
        locationIter.setPos(elseBodyLocations.head.locationIndex)
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

      println("START OF RETREIVE>>>>> " + loc.locationIndex + " :: " + loc.locationUri)

      loc.statement match {
        case gs: GotoStatement =>
          val gotoLocation = gs.targetLocation.locationIndex
          //Adding goto in in block
          addLocation(loc, ifBodyLocations )

          // This is a forward jump case. Add the location and follow the jump statement.
          if(gotoLocation > loc.locationIndex) {
            addLocation(loc, ifBodyLocations )
            retrieveIfBodyLocations(ifBodyLocations, elseBodyLocations, startLocation, originalTargetLocation, locationIter, currentState)
          }
          // Backward jump, but after the initial If location
          // Add the location and all locations from initial If statement (@startLocation) to the current goto target location(@gotoLocation)
          else if(gotoLocation < loc.locationIndex && gotoLocation > startLocation) {
            val addLocations = locationIter.locations.filter (l => l.locationIndex > startLocation && l.locationIndex < gotoLocation )
            //          addLocations.foreach(l =>println(l.locationUri + " :: " + l.locationIndex))
            elseBodyLocations ++= addLocations
          }
          // Backward jump, but jumps even before the initial If location
          // Add the location and all locations from initial If location(@startLocation) to the initial target location (originalTargetLocation)
          //This could be a loop
          else if (gotoLocation <= startLocation) {

            val addLocations = locationIter.locations.filter (l => l.locationIndex > startLocation && l.locationIndex < originalTargetLocation )
            elseBodyLocations ++= addLocations
          }

        case rs: ReturnStatement =>
          val addLocations = locationIter.locations.filter (l => l.locationIndex > startLocation && l.locationIndex < originalTargetLocation )

          // Location limit can be due to a If statement as well as it also has a goto part.
          //        val locationLimit: Option[Location] = addLocations find (al => al.statement.isInstanceOf[GotoStatement])

          removeLocationsFromIfBlock(addLocations)
          //        addLocations.foreach(l =>println("Adding to else block: " + l.locationUri))
          elseBodyLocations ++= addLocations

        case _ =>
          addLocation(loc, ifBodyLocations)
          retrieveIfBodyLocations(ifBodyLocations, elseBodyLocations, startLocation, originalTargetLocation, locationIter, currentState)
      }

      //    if(elseBodyLocations.nonEmpty && ifBodyLocations.isEmpty) {
      if(elseBodyLocations.nonEmpty ) {
        println("Checking else body for loop for if Statement at.: " + startLocation)
        //        ifBodyLocations.foreach(l => println("IF BODY locations are: " + l.locationUri + " :: " + l.locationIndex))
        //        elseBodyLocations.foreach(l => println("ELSE BODY locations are: " + l.locationUri + " :: " + l.locationIndex))
        //last statement may not be a goto. Find the last goto.

        val lastGoto = elseBodyLocations.lastIndexWhere(l => l.statement.isInstanceOf[GotoStatement])
        if (lastGoto != -1) {
          //          if(gs.targetLocation.locationIndex <= startLocation) {
          val gotoLocation: Location =  elseBodyLocations(lastGoto)
          val gs: GotoStatement = gotoLocation.statement.asInstanceOf[GotoStatement]
          println("Goto is at: " + gotoLocation.locationUri + " :: " + gotoLocation.locationIndex)

          val locationsToRemove = elseBodyLocations filter (l=> l.locationIndex > gotoLocation.locationIndex)
          println("ABOUT TO REMOVING LOCATION FROM ELSE BODY!!!" + locationsToRemove)

          for (r <- locationsToRemove) {
            println("REMOVING LOCATION FROM ELSE BODY!!!")
            elseBodyLocations.remove(elseBodyLocations.indexOf(r))
          }

          println("ADDING TO PARENT>>>>")

          val currentLocation = locationIter.pos
          //Setting position to start of the else body for identifying the loop.
          locationIter.setPos(elseBodyLocations.head.locationIndex)
          identifyLoop(startLocation, locationIter, currentState, elseBodyLocations)
          if(!currentState.isLoop) {
            currentState.additionalState = IfState(startLocation)
          }
          //need to add removed statements to the parent block.
          /* currentState.parentState match {
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
           }*/
          /*currentState.parentTask match {
            case Some(p) =>

              if(currentState.isLoop) {
                ifBodyLocations.foreach{
                  l =>
                    println("ADDING IF BODY IN LOOP TO PARENT>>>" + l.locationIndex + " :: " + l.locationUri)
//                    p.addToParent += l
                    p.addToParent(l)
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
          }*/
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
              //            ifBodyLocations.foreach(l => println("IfbodyLocations After removal are: " + l.locationIndex + " :: " + l.locationUri))
            }

          case None =>
        }
      }
    }

    def identifyDoLoop (startLocation: Int,
                        locationIter: LocationIterator,
                        currentState: CurrentState,
                        elseBodyLocations: MList[Location],
                        locationsToAdd: MSet[Location] = msetEmpty): Unit = {
      val loc = locationIter.next()

      println("Identifying DO loop... for if at: " + startLocation)
      println("Identifying DO loop... Current location is : " + loc.locationIndex + " :: " + loc.locationUri)
      loc.statement match {
        case gs: GotoStatement =>
          println("Checking DO loop Goto Statement. If it is not a backward jump, this is not a loop??? =>>> This is used in the Else Body evaluation Case. This evaluates the final goto statement.")
          //todo check if we need this || part in gotoStatement
          //        if(gs.targetLocation.locationIndex < loc.locationIndex) {
          println("Do Loop Goto Statement Info: -target: " + gs.targetLocation.locationIndex + " :: " + gs.targetLocation.location)
          println("Do Loop Goto Statement Info: -gotoLoc: " +  loc.locationIndex + " :: " +  loc.locationUri)
          //        currentState.endOfLoop = loc
          currentState.endOfLoop = loc.locationIndex
          if(gs.targetLocation.locationIndex < loc.locationIndex || locationIter.locations.exists(l => l.locationIndex == gs.targetLocation.locationIndex)) {
            locationIter.setPos(gs.targetLocation.locationIndex)
          }
        case rs: ReturnStatement =>
          println ("Not a Do loop : Return")
          return

        case is: IfStatement =>
          println("Identifying Do loop if statement.")
          if(loc.locationIndex == startLocation) {
            println("This is a DO loop")
            currentState.isLoop = true

            //Setup Loop State:
            currentState.additionalState = LoopState(startLocation)

            elseBodyLocations ++= locationsToAdd
            currentState.loopInitialisers ++= locationsToAdd
            println("adding to elsebody Do loop" + loc.locationIndex + " :: " + loc.locationUri)
            //          elseBodyLocations.foreach(l => println(l.locationIndex + " :: " + l.locationUri))
            return
          }
          //           else if (currentState.parentState.isDefined) {
          else if (currentState.parentTask.isDefined) {
            println("identifyDo  loop ifStatement else part.")
            currentState.parentTask match {
              case Some(p) =>
                //              if(is.targetLocation.locationIndex < startLocation || locationIter.locations.exists(l => l.locationIndex == is.targetLocation.locationIndex)) {
                if(is.targetLocation.locationIndex > loc.locationIndex && locationIter.locations.exists(l => l.locationIndex == is.targetLocation.locationIndex)) {
                  //                if(is.targetLocation.locationIndex > loc.locationIndex && elseBodyLocations.exists(l => l.locationIndex == is.targetLocation.locationIndex)) {
                  locationIter.setPos(is.targetLocation.locationIndex)
                  println("INSIDE PARENT Do llop!!!!!" + is.targetLocation.locationIndex + " :: " + loc.locationIndex )
                }
              case None =>
                if(is.targetLocation.locationIndex < startLocation) {
                  locationIter.setPos(is.targetLocation.locationIndex)
                }
            }
          } else {
            if(is.targetLocation.locationIndex > loc.locationIndex) {
              locationIter.setPos(is.targetLocation.locationIndex)
            }
          }
        /*else {
        println("identify loop ifStatement else part!!!! "  + "   :::   " + currentState.endOfLoop)
        if((is.targetLocation.locationIndex < startLocation && currentState.endOfLoop != -1) || (is.targetLocation.locationIndex > loc.locationIndex )) {
        println("INSIDE IDENTIFY DO LOOP ELSE PART")
        locationIter.setPos(is.targetLocation.locationIndex)

        }
        }*/


        case _ =>
          if(!elseBodyLocations.contains(loc)) {
            println("adding to addlocations Do loop " + loc.locationIndex + " :: " + loc.locationUri)
            //          elseBodyLocations += loc
            locationsToAdd += loc

            //          elseBodyLocations.foreach(l => println(l.locationIndex + " :: " + l.locationUri))
            //            locationsToAdd.foreach(l => println(l.locationIndex + " :: " + l.locationUri))
            //Resetting visited.
            currentState.resetVisitedLocation(loc.locationIndex)
          }
      }

      //Moved This outside so that this happens everytime now that we are checking if body from top to bottom.
      if(locationIter.hasNext) {
        identifyDoLoop(startLocation, locationIter, currentState, elseBodyLocations, locationsToAdd)
      }

    }

    /** Follow the goto to check if the original IfStatement
      * OR if the parent is a loop, then see if parents if can be reached or not. If it can be reached, then it is a loop.
      */
    def identifyLoop (startLocation: Int,
                      locationIter: LocationIterator,
                      currentState: CurrentState,
                      elseBodyLocations: MList[Location],
                      locationsToAdd: MSet[Location] = msetEmpty): Unit = {
      val loc = locationIter.next()

      println("Identifying loop... for if at: " + startLocation)
      println("Identifying loop... Current location is : " + loc.locationIndex + " :: " + loc.locationUri)
      loc.statement match {
        case gs: GotoStatement =>
          println("Checking Goto Statement. If it is not a backward jump, this is not a loop??? =>>> This is used in the Else Body evaluation Case. This evaluates the final goto statement.")
          //todo check if we need this || part in gotoStatement
          println("Goto Statement Info: -target: " + gs.targetLocation.locationIndex + " :: " + gs.targetLocation.location)
          println("Goto Statement Info: -gotoLoc: " +  loc.locationIndex + " :: " +  loc.locationUri)
          currentState.endOfLoop = loc.locationIndex
          if(gs.targetLocation.locationIndex < loc.locationIndex || locationIter.locations.exists(l => l.locationIndex == gs.targetLocation.locationIndex)) {
            locationIter.setPos(gs.targetLocation.locationIndex)
          }

        case rs: ReturnStatement =>
          println ("Not a loop : Return")
          return

        case is: IfStatement =>
          println("Identifying loop if statement.")
          if(loc.locationIndex == startLocation){
            println ("This is a loop")
            println("END OF LOOP IS: " + currentState.endOfLoop)
            currentState.isLoop = true

            //Setup Loop State:
            currentState.additionalState = LoopState(startLocation)

            elseBodyLocations ++= locationsToAdd
            currentState.loopInitialisers ++= locationsToAdd
            //            locationsToAdd.foreach(l=> println("adding to elsebody" + l.locationIndex + " :: " + l.locationUri))
            //          elseBodyLocations.foreach(l => println(l.locationIndex + " :: " + l.locationUri))
            return
          } else {
            println("identify loop ifStatement else part!!!! "  + "   :::   " + currentState.endOfLoop)
            if((is.targetLocation.locationIndex < startLocation && currentState.endOfLoop != -1) || (is.targetLocation.locationIndex > loc.locationIndex )) {
              println("INSIDE")
              locationIter.setPos(is.targetLocation.locationIndex)

            }
          }

        case _ =>
          if(!elseBodyLocations.contains(loc)) {
            println("adding to addlocations" + loc.locationIndex + " :: " + loc.locationUri)
            locationsToAdd += loc
            locationsToAdd.foreach(l => println(l.locationIndex + " :: " + l.locationUri))
            //Resetting visited.
            currentState.resetVisitedLocation(loc.locationIndex)
          }
      }

      //Moved This outside so that this happens everytime now that we are checking if body from top to bottom.
      if(locationIter.hasNext) {
        identifyLoop(startLocation, locationIter, currentState, elseBodyLocations, locationsToAdd)
      }
    }

    def visitSimpleStatement(imports: MSet[JawaType],
                             bodyStatements: MList[(Int, ST)],
                             isConstructor: Boolean,
                             thisParam: Option[Param],
                             loc: Location,
                             statement: Statement,
                             currentState: CurrentState): Unit = {
      println ("current location is : " + loc.locationIndex + " :: " + loc.locationSymbol.location)
      if(currentState.getVisitedCount(loc.locationIndex) > 0) {
        println ("this location has already been visited: " + loc.locationIndex + " :: " + loc.locationUri)
        return
      }

      statement match {
        case as: AssignmentStatement =>
          bodyStatements += ((loc.locationIndex, visitAssignmentStatement(as, thisParam, imports)))

        case rs: ReturnStatement =>
          rs.varOpt match {
            case Some(v) =>
              val returnTemplate = template.getInstanceOf("ReturnStatement")
              returnTemplate.add("varName", v.varName)

              //todo use parentTAsk
              /*if(!currentState.isIfStatement && !currentState.isElseIfStatement && !currentState.isElseStatement) {
                println ("This is the end of the program. No need to parse others. Setting iterator to end" )
                locationIter.setPos(locationIter.locations.length - 1)
              }*/

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

        case _ =>
      }
      //todo pick one??
      //    mainIter.visitLocation(loc.locationIndex)
      currentState.visitLocation(loc.locationIndex)
    }
  }

  trait Task {
    var isResolved: Boolean = false
    var isLevel1: Boolean = true // use this to filter nested lines within some other block.
    def resolve(bodyStatements: MList[(Int, ST)]): Unit
  }

  trait BlockTask extends Task {
    //    val taskMap: MMap[Int, Task] = mmapEmpty
    val taskMap: mutable.LinkedHashMap[Int, Task] = mlinkedMapEmpty
    val level: Int
    var endOfBlock: Int = _
    //    val currentState: CurrentState
    //    def resolve(): Unit
    def identifyTasks(): Unit

    //    def checkLocation(locationIndex: Int): Boolean
    def checkLocation(locationIndex: Int): Boolean = taskMap.exists{case(loc, _)=> loc == locationIndex}

    val visitedLocations: MMap[Int, Int] = mmapEmpty

    def visitLocation(locationIndex: Int): Unit = {
      visitedLocations(locationIndex) = visitedLocations.getOrElse(locationIndex, 0) + 1
    }

    def getVisitedCount(locationIndex: Int): Int = {
      visitedLocations.getOrElse(locationIndex, 0)
    }

    def resetVisitedLocation(locationIndex: Int): Unit = {
      visitedLocations(locationIndex) = 0
    }
  }

  case class MainTask(locations: IList[Location],
                      thisParam: Option[Param],
                      isConstructor: Boolean) extends BlockTask with TaskHelper {

    val level = 0
    val resolvedTaskMap: IMap[Int, ST] = imapEmpty

    val locationIterator: LocationIterator = LocationIterator(Left(locations))

    val currentState = CurrentState(isConstructor = isConstructor, isIfStatement = false, isElseIfStatement = false, isElseStatement = false, targetLocation = null)

    def identifyTasks(): Unit = {
      if (locationIterator.hasNext) {

        val loc: Location = locationIterator.next()
        //        val currentTask: Task = identifyTask(loc, locationIterator, this, this)
        val currentTask: Task = identifyTask(loc, locationIterator, this, this, this)
        taskMap += loc.locationIndex -> currentTask

        currentTask match {
          case bt: BlockTask =>
            //            println("CALLING NESTED BLOCK TASK IDENTIFIER FROM MAIN!")
            bt.identifyTasks()
            //            println("Current Task in Main Block is a Block Task" + bt.getClass)
            //            println("End of Block is: " + bt.endOfBlock)
            locationIterator.setPos(bt.endOfBlock)
          case _ =>
        }

        identifyTasks()
      }
    }

    def taskExists(locationIndex: Int): Boolean = taskMap.contains(locationIndex)

    override def checkLocation(locationIndex: Int): Boolean = locations.exists(loc=> loc.locationIndex == locationIndex)

    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      //      val bodyStatements: MList[(Int, ST)] = mlistEmpty
      taskMap.toSeq.sortBy(_._1) foreach {
        case (loc, task) =>
          task.resolve(bodyStatements)
      }
    }

  }

  case class SimpleTask(location: Location,
                        mainTask: MainTask) extends Task with TaskHelper{

    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      println("Resolving Simple Task: " + location.locationIndex)
      //      val fakeParam: Option[Param] = None
      //      visitSimpleStatement(msetEmpty, bodyStatements, false,  fakeParam, location, location.statement, CurrentState())
      visitSimpleStatement(msetEmpty, bodyStatements, mainTask.isConstructor,  mainTask.thisParam, location, location.statement, CurrentState())
    }
  }

  case class IfElseTask(location: Location,
                        ifBodyLocations: MList[Location],
                        elseBodyLocations: MList[Location],
                        mainTask: MainTask,
                        parentTask: BlockTask) extends BlockTask with TaskHelper {
    val level: Int = parentTask.level + 1

    val ifTask: IfTask = IfTask(location, ifBodyLocations, mainTask, parentTask)
    val elseTask: IfTask = IfTask(location, elseBodyLocations, mainTask, parentTask, isElse = true)

    def identifyTasks(): Unit = {
      println("Identifying If Block Tasks")
      ifTask.identifyTasks()
      println("Identifying Else Block Tasks")
      elseTask.identifyTasks()

      taskMap ++= ifTask.taskMap
      taskMap ++= elseTask.taskMap
    }

    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      println("RESOLVING IF TASK")
      ifTask.resolve(bodyStatements)

      println("RESOLVING ELSE TASK")
      elseTask.resolve(bodyStatements)
    }
  }

  case class IfTask(location: Location,
                    ifBodyLocations: MList[Location],
                    mainTask: MainTask,
                    parentTask: BlockTask,
                    isElse: Boolean = false) extends BlockTask with TaskHelper {
    val level: Int = parentTask.level + 1
    val ifStatement: IfStatement = location.statement.asInstanceOf[IfStatement]

    val locationIterator: LocationIterator = LocationIterator(Right(ifBodyLocations))

    def identifyTasks(): Unit = {
      println("Identifying If Block Tasks: isElse: " + isElse)

      if (locationIterator.hasNext) {

        val loc: Location = locationIterator.next()
        //        val currentTask: Task = identifyTask(loc, locationIterator, mainTask, this, Some(parentTask))
        val currentTask: Task = identifyTask(loc, locationIterator, mainTask, this, parentTask)
        taskMap += loc.locationIndex -> currentTask

        currentTask match {
          case bt: BlockTask =>
            bt.identifyTasks()
            locationIterator.setPos(bt.endOfBlock)
          case _ =>
        }
        //todo pick one??
        //    mainIter.visitLocation(loc.locationIndex)
        //        currentState.visitLocation(loc.locationIndex)
        //todo use later while translating
        //        visitLocation(loc.locationIndex)
        identifyTasks()
      }
    }

    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      println("RESOLVING IF TASK")
      val ifBodyStatements: MList[(Int, ST)] = mlistEmpty
      //      taskMap.toSeq.sortBy(_._1) foreach {
      taskMap foreach {
        case (loc, task) =>
          //          println("Resolving IF task: "+loc+ " :: " + task)
          if(getVisitedCount(loc) == 0) {
            task.resolve(ifBodyStatements)
            visitLocation(loc)
          }
      }

      parentTask.visitedLocations ++= visitedLocations

      val ifTemplate: ST = template.getInstanceOf("IfStatement")

      if(isElse) {
        ifTemplate.add("token", "else")
      } else {
        ifTemplate.add("token", ifStatement.ifToken.text)
        ifTemplate.add("cond", visitBinaryExpression(ifStatement.cond))
      }

      val ifBodyTemplate = template.getInstanceOf("Body")
      ifBodyStatements.sortBy(_._1).map {
        st =>
          ifBodyTemplate.add("statements", st._2)
      }
      ifTemplate.add("body", ifBodyTemplate)

      bodyStatements += ((location.locationIndex + (if(isElse) 1 else 0), ifTemplate))
    }

    override def checkLocation(locationIndex: Int): Boolean = ifBodyLocations.exists(loc=> loc.locationIndex == locationIndex)

  }

  case class WhileTask(location: Location,
                       loopBodyLocations: MList[Location],
                       mainTask: MainTask,
                       parentTask: BlockTask) extends BlockTask with TaskHelper {
    val locationIterator: LocationIterator = LocationIterator(Right(loopBodyLocations))
    val ifStatement: IfStatement = location.statement.asInstanceOf[IfStatement]

    val level = parentTask.level + 1

    def identifyTasks(): Unit = {
      println("Identifying While Block Tasks")

      if (locationIterator.hasNext) {
        val loc: Location = locationIterator.next()
        //        val currentTask: Task = identifyTask(loc, locationIterator, mainTask, this, Some(parentTask))
        //        val currentTask: Task = identifyTask(loc, locationIterator, mainTask, this, Some(parentTask))
        val currentTask: Task = identifyTask(loc, locationIterator, mainTask, this, parentTask)
        //        println("Identifying Current While Task at " + location.locationIndex + "  ::  " + location.locationUri + "is: " + currentTask)
        taskMap += loc.locationIndex -> currentTask

        currentTask match {
          case bt: BlockTask =>
            bt.identifyTasks()
            //            println("Identified While Block Tasks. at " + location.locationIndex + "  ::  " + location.locationUri + " end of block is: " + bt.endOfBlock)
            locationIterator.setPos(bt.endOfBlock)
          case _ =>
        }
        //todo pick one??
        //    mainIter.visitLocation(loc.locationIndex)
        //        currentState.visitLocation(loc.locationIndex)
        //todo use later while translating
        //        visitLocation(loc.locationIndex)
        identifyTasks()
      } else {
        println("END OF  While Block Tasks. at " + location.locationIndex + "  ::  " + location.locationUri + " end of block is: " + endOfBlock)
      }
    }

    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      println("RESOLVING WHILE TASK at : "+ location.locationIndex + " :: " + location.locationUri)
      val loopBodyStatements: MList[(Int, ST)] = mlistEmpty

      //      taskMap foreach {case(l, v) => println("WHile task Map is : " + l+ v) }
      //      taskMap.toSeq.sortBy(_._1) foreach {
      breakable {
        taskMap foreach {
          case (loc, task) =>
            //          println("Resolving While task at : "+ location.locationIndex + " :: " + location.locationUri +"current TAsk is: " + loc + " :: " + task)
            //          task.resolve(loopBodyStatements)
            if(getVisitedCount(loc) == 0 && loc <= endOfBlock) {
              task.resolve(loopBodyStatements)
              visitLocation(loc)
            }
          /*if(loc == endOfBlock) {
            break
          }*/
        }
      }


      parentTask.visitedLocations ++= visitedLocations
      val loopTemplate: ST = template.getInstanceOf("WhileLoop")

      loopTemplate.add("cond", visitBinaryExpression(ifStatement.cond))


      val loopBodyTemplate = template.getInstanceOf("Body")
      loopBodyStatements map {
        st =>
          loopBodyTemplate.add("statements", st._2)
      }
      loopTemplate.add("body", loopBodyTemplate)

      bodyStatements += ((location.locationIndex, loopTemplate))
    }

    override def checkLocation(locationIndex: Int): Boolean = {
      println("CHECKING LOCATION IN WHILE LOOP>>>>>")
      loopBodyLocations.exists(loc=> loc.locationIndex == locationIndex)
    }
  }

  case class DoWhileTask(location: Location,
                         loopBodyLocations: MList[Location],
                         mainTask: MainTask,
                         parentTask: BlockTask) extends BlockTask with TaskHelper {
    val level = parentTask.level + 1
    val ifStatement: IfStatement = location.statement.asInstanceOf[IfStatement]
    val locationIterator: LocationIterator = LocationIterator(Right(loopBodyLocations))

    def identifyTasks(): Unit = {
      println("Identifying Do While Block Tasks")
      //todo mark others isLevel1 = false. Put task in the first line of loop. Incorporate all loop statements in this task.
    }

    /*def resolve(): Unit = {
      taskMap foreach {
        t =>
          println("Resolving Do...While Task: Current Line: ")
      }
    }*/

    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      println("Resolving Do...While Task")
      val loopBodyStatements: MList[(Int, ST)] = mlistEmpty

      taskMap.toSeq.sortBy(_._1) foreach {
        case (loc, task) =>
          //          println("Resolving task: "+loc+ " :: " + task)
          //          task.resolve(loopBodyStatements)
          if(getVisitedCount(loc) == 0) {
            task.resolve(loopBodyStatements)
            visitLocation(loc)
          }
      }

      parentTask.visitedLocations ++= visitedLocations
      val doWhileTemplate = template.getInstanceOf("DoWhile")
      doWhileTemplate.add("while", visitBinaryExpression(ifStatement.cond))

      val loopBodyTemplate = template.getInstanceOf("Body")
      loopBodyStatements map {
        st =>
          loopBodyTemplate.add("statements", st._2)
      }
      doWhileTemplate.add("body", loopBodyTemplate)
      bodyStatements += ((location.locationIndex, doWhileTemplate))
    }

    override def checkLocation(locationIndex: Int): Boolean = loopBodyLocations.exists(loc=> loc.locationIndex == locationIndex)

  }

  case class LocationIterator (eitherLoc: Either[IList[Location], MList[Location]]) {
    var locations: List[Location] = eitherLoc match {
      case Left(il) => il
      case Right(rl) => rl.toList
    }

    var pos = 0

    def next(): Location = {
      val current: Location = locations(pos)
      pos += 1
      current
    }

    def hasNext: Boolean = {
      pos >= 0 && pos < locations.length
    }

    def setPos(locationIndex: Int): Unit = {
      pos = locations.indexWhere(l=> l.locationIndex == locationIndex)
    }
  }

  //todo Clean up
  case class CurrentState(
                           isConstructor: Boolean = false,
                           var isIfStatement: Boolean = false,
                           var isElseIfStatement: Boolean = false,
                           var isElseStatement: Boolean = false,
                           var isLoop: Boolean = false,
                           var isDoWhile: Boolean = false,
                           var targetLocation: String = "",
                           var locationOffset: Int = 0,
                           parentState: Option[CurrentState] = None,
                           parentTask: Option[BlockTask] = None) {
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

    var additionalState: AdditionalState = _
    //    var endOfLoop: Location = _
    var endOfLoop: Int = -1
    var loopInitialisers: MSet[Location] = msetEmpty

    var addToParent: MList[Location] = mlistEmpty
  }

  trait AdditionalState

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

        val mainTask = MainTask(resolvedBody.locations, md.thisParam, isConstructor)

        mainTask.identifyTasks()
        println("tasks are: " + mainTask.taskMap )
        mainTask.taskMap.foreach{
          case(k, v) =>
            println("location: " + k )
            v match {
              case bt: BlockTask =>
                bt match {
                  case iet: IfElseTask =>
                    println("IFTASK")
                    iet.ifBodyLocations.foreach(l=> println(l.locationIndex+ " :: " + l.locationUri))
                    println("ELSETASK")
                    iet.elseBodyLocations.foreach(l=> println(l.locationIndex+ " :: " + l.locationUri))
                  case wt: WhileTask =>
                    println("WHILE")
                    wt.loopBodyLocations.foreach(l=> println(l.locationIndex+ " :: " + l.locationUri))
                  case wt: DoWhileTask =>
                    println("DO WHILE")
                    wt.loopBodyLocations.foreach(l=> println(l.locationIndex+ " :: " + l.locationUri))
                }
              case st: SimpleTask =>
                println(st.location.locationIndex + " :: " + st.location.locationUri)
            }
        }

        val bodyStatements: MList[(Int, ST)] = mlistEmpty
        mainTask.resolve(bodyStatements)

        val bodyTemplate = template.getInstanceOf("Body")
        bodyStatements.sortBy(_._1).map {
          st =>
            bodyTemplate.add("statements", st._2)
        }
        methodTemplate.add("body", bodyTemplate)

      /*val thisParam: Option[Param] = md.thisParam
      val locationIter = LocationIterator(Left(resolvedBody.locations))
      val currentState = CurrentState(isConstructor = isConstructor, isIfStatement = false, isElseIfStatement = false, isElseStatement = false, targetLocation = null)

      if (locationIter.hasNext) visitLocation(imports, bodyStatements, isConstructor, thisParam, locationIter, currentState)*/

      case UnresolvedBody(bodytokens) =>
    }


    /*val bodyTemplate = template.getInstanceOf("Body")
    bodyStatements.sortBy(_._1).map {
      st =>
        bodyTemplate.add("statements", st._2)
    }
    methodTemplate.add("body", bodyTemplate)*/
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

        //        val (ifBodyLocations: MList[Location], elseBodyLocations: MList[Location]) = prepareIfBodyStatements(imports, bodyStatements, isConstructor, thisParam, mainIter, loc, ifCurrentState, ifStatement)
        val (ifBodyLocations: MList[Location], elseBodyLocations: MList[Location]) = (mlistEmpty[Location], mlistEmpty[Location])

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
          println("VISITING LOOP")
          visitLoopStatement(imports, bodyStatements, isConstructor, thisParam, elseLocationIter, loc, ifCurrentState, ifStatement, mainIter, "loop")
        }
        else if (ifCurrentState.isDoWhile) {
          val elseLocationIter = LocationIterator(Right(elseBodyLocations))
          println("VISITING DO WHILE LOOP")
          elseBodyLocations.foreach(l=> println(l.locationIndex + " :: " + l.locationUri))
          //          val first = bodyStatements.find(b=> b._1 == elseBodyLocations.head.locationIndex)
          val bs = bodyStatements

          bodyStatements.find(b=> b._1 == elseBodyLocations.head.locationIndex) match {
            case Some(b) =>
              val doTemplate = template.getInstanceOf("DoLoop")
              doTemplate.add("do", b._2)

              println("Location For Do While Template: " + bodyStatements.indexOf(b))
              bodyStatements(bodyStatements.indexOf(b)) = (b._1, doTemplate)
            case None =>
          }

          val doWhileTemplate = template.getInstanceOf("DoWhile")
          doWhileTemplate.add("while", visitBinaryExpression(ifStatement.cond))

          println("end of do while:  " + locationIter.pos)
          println("end of do while:  " + loc.locationIndex)

          bodyStatements += ((loc.locationIndex, doWhileTemplate))
          //          visitLoopStatement(imports, bodyStatements, isConstructor, thisParam, elseLocationIter, loc, ifCurrentState, ifStatement, mainIter, "loop")
        }
        else {
          //          currentState.endOfLoop = null
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
        //july `8 : adding condition for do...while loop
        if(elseBodyLocations.nonEmpty) {
          if(!ifCurrentState.isDoWhile) {
            //          mainIter.setPos(elseBodyLocations.last.locationIndex + 1)
            locationIter.setPos(elseBodyLocations.last.locationIndex + 1)

            println("END OF Visit Statement end of do while:  " + locationIter.pos)
            println("END OF Visit Statement end of do while:  " + loc.locationIndex)
            currentState.nextLocation = elseBodyLocations.last
          }
        }

      case _ =>
    }
    //todo pick one??
    //    mainIter.visitLocation(loc.locationIndex)
    currentState.visitLocation(loc.locationIndex)
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
          //          val originalLocation = locationIter.pos

          visitStatement(imports, ifBodyStatements, isConstructor, thisParam, mainIter, loc, statement, currentState, mainIter)

          //todo where to set this position??
          locationIter.setPos(originalLocation + 1)
          //          locationIter.setPos(originalLocation)
          locationIter.locations.foreach(l => println("if iterator locations: " + l.locationIndex+ " :: " + l.locationUri))
          println("POS is: " + locationIter.pos)
          println("POS is: " + locationIter.pos)
        //          println("new Index pointed to: " + newCurrentState.nextLocation.locationIndex + " :: " + newCurrentState.nextLocation.locationUri)
        //          println("size of location iter: " + locationIter.locations.size )
        //          locationIter.setPos(locationIter.locations.indexOf(newCurrentState.nextLocation) + 1)

        case _ =>
          visitStatement(imports, ifBodyStatements, isConstructor, thisParam, locationIter, loc, statement, currentState, mainIter)
      }
    }

    if(locationIter.hasNext){
      //      visitNewIfBodyLocation(imports, ifBodyStatements, isConstructor, thisParam, locationIter, currentState, bodyStatements)
      //      if((currentState.isLoop && loc != currentState.endOfLoop) || !currentState.isLoop) {
      //      if(!currentState.isLoop || (currentState.isLoop && loc != currentState.endOfLoop)) {
      if(!currentState.isLoop || (currentState.isLoop && loc.locationIndex != currentState.endOfLoop)) {
        visitNewIfBodyLocation(imports, ifBodyStatements, isConstructor, thisParam, locationIter, currentState, bodyStatements, mainIter)
        //      } else if(currentState.isLoop && loc == currentState.endOfLoop && currentState.loopInitialisers.nonEmpty){
      } else if(currentState.isLoop && loc.locationIndex == currentState.endOfLoop && currentState.loopInitialisers.nonEmpty){
        val newLocationIter: LocationIterator = LocationIterator(Left(currentState.loopInitialisers.toList))
        visitNewIfBodyLocation(imports, ifBodyStatements, isConstructor, thisParam, newLocationIter, currentState, bodyStatements, mainIter)
      }
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
