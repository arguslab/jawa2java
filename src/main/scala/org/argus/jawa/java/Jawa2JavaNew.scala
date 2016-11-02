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
    def identifyTask(loc: Location, locationIterator: LocationIterator, mainTask: MainTask,
                     currentTask: BlockTask, parentTask: BlockTask): Task = {
      val statement: Statement = loc.statement

      statement match {
        case ifStatement: IfStatement =>
          val ifCurrentState = CurrentState(isConstructor = mainTask.isConstructor, parentTask = Some(currentTask))

          val (ifBodyLocations: MList[Location], elseBodyLocations: MList[Location]) = prepareIfBodyStatements(
            mainTask.locationIterator, loc, ifCurrentState, ifStatement)
          if (ifCurrentState.isLoop) {
            val whileTask = WhileTask(loc, elseBodyLocations, mainTask, currentTask)
            whileTask.endOfBlock = ifCurrentState.endOfLoop
            whileTask.loopInitialisers ++= ifCurrentState.loopInitialisers
            whileTask
          }
          else if (ifCurrentState.isPartOfBlock) {
            val startOfLoop: Int = ifStatement.targetLocation.locationIndex
            var parentParadox: Boolean = false
            // validation for do...while loops.
            // Current Task is the parent for this task.
            currentTask match {
              case wt: WhileTask =>
                //If goto target of do...while is before its parent, the parent is within the do...while loop.
                if(wt.startOfLoop > startOfLoop) {
                  wt.isPartOfBlock = true
                  //Clean the parent iterator
                  wt.locationIterator.locations = wt.locationIterator.locations filter (
                      l=> l.locationIndex < startOfLoop)
                }

          /*    case dwt: DoWhileTask =>
                println("PARENT OF DO... WHILE Is DO WHILE.")   //Debug*/

              case it: IfElseTask=>
                //If goto target of do...while is before its parent, the parent is within the do...while loop.
                if(it.location.locationIndex > startOfLoop) {
                  //Removing locations from parent of Do... while
                  parentParadox = true
                  it.isPartOfBlock = true
                  //Added this as parent is now IfElseTask
                  it.ifTask.isPartOfBlock = true
                  it.elseTask.isPartOfBlock = true
                  // Clean the parent iterator
                  // todo Changed ifTask to IfElseTask: Need to check if both if and else parts need filtering.
                  // it.locationIterator.locations = it.locationIterator.locations filter (
                  // l=> l.locationIndex < startOfLoop)
                  it.ifTask.locationIterator.locations = it.ifTask.locationIterator.locations filter (
                      l=> l.locationIndex < startOfLoop)
                  it.elseTask.locationIterator.locations = it.elseTask.locationIterator.locations filter (
                      l=> l.locationIndex < startOfLoop)
                }
              case _ =>
//                println("CurrentTask is: "+ currentTask.getClass) //Debug
            }

            if (!parentParadox) {
              val doWhileTask = DoWhileTask(loc, elseBodyLocations, mainTask, currentTask)
              doWhileTask.endOfBlock = loc.locationIndex + 1
              doWhileTask
            } else {
              NoneTask()
            }
          }
          else {
            val ifElseTask = IfElseTask(loc, ifBodyLocations, elseBodyLocations, mainTask, currentTask)
            ifElseTask.endOfBlock = elseBodyLocations.last.locationIndex
            ifElseTask
          }

        case gs: GotoStatement =>
          // Check goto target for switch statement.
          retrieveLocation(gs.targetLocation.locationIndex, mainTask.locationIterator).statement match {
            case ss: SwitchStatement =>
              val switchCases: MLinkedMap[String, MList[Location]] = mlinkedMapEmpty
              val switchCurrentState = CurrentState(isConstructor = mainTask.isConstructor,
                parentTask = Some(currentTask))
              val defaultTarget: Int = ss.defaultCaseOpt match {
                case Some(d) => d.targetLocation.locationIndex
                case None => -1
              }
              // Switch Cases
              ss.cases foreach {
                c =>
                  if(c.targetLocation.locationIndex != defaultTarget) {
                    val caseLocations: MList[Location] = mlistEmpty
                    mainTask.locationIterator.setPos(c.targetLocation.locationIndex)
                    retrieveCaseStatements(caseLocations, mainTask.locationIterator, switchCurrentState)
                    switchCases += c.constant.rawtext -> caseLocations
                  }
              }
              //Default Case
              val defaultCase: Option[MList[Location]] = ss.defaultCaseOpt match {
                case Some(d) =>
                  val caseLocations: MList[Location] = mlistEmpty
                  mainTask.locationIterator.setPos(d.targetLocation.locationIndex)
                  retrieveCaseStatements(caseLocations, mainTask.locationIterator, switchCurrentState)
                  Some(caseLocations)

                case None => None
              }

              mainTask.locationIterator.setPos(loc.locationIndex + 1)

              val switchTask = SwitchTask(loc, ss, switchCases, defaultCase,  mainTask, parentTask)
              //todo Find end of block logic for switch statement
              switchTask.endOfDefault = switchCurrentState.endOfLoop
              switchTask.endOfBlock = mainTask.locationIterator.pos
              switchTask

            case _ =>
              val st: SimpleTask = SimpleTask(loc, mainTask)
              checkTryCatchBlock(mainTask, currentTask, st, loc)
              st
          }

        case _ =>
          // add to tryCatch here.
          val st: SimpleTask = SimpleTask(loc, mainTask)
          checkTryCatchBlock(mainTask, currentTask, st, loc)
          st
      }
    }

    def checkTryCatchBlock(mainTask: MainTask,
                           currentTask: BlockTask,
                           st: SimpleTask,
                           loc: Location): Unit = {
      mainTask.tryCatchTaskMap foreach {
        case (idx, tct) =>
          if (tct.tryBodyLocations.contains(loc)) {
            currentTask match {
              case mt: MainTask =>
                //SIMPLE STATEMENT TRY
                tct.tryTask.taskMap += loc.locationIndex -> st
                mainTask.taskMap.remove(loc.locationIndex)
                st.isPartOfTryCatch = true

              case _ =>
                //BLOCK STATEMENT TRY  " + currentTask.locationIndex + " :: :: " + currentTask.getClass
                tct.tryTask.taskMap += currentTask.locationIndex -> currentTask
                mainTask.taskMap.remove(loc.locationIndex)
                st.isPartOfTryCatch = true
                currentTask.isPartOfTryCatch = true
            }
          } else if (tct.catchBodyLocations.contains(loc)) {
            currentTask match {
              case mt: MainTask =>
                //SIMPLE STATEMENT CATCH
                tct.catchTask.taskMap += loc.locationIndex -> st
                mainTask.taskMap.remove(loc.locationIndex)
                st.isPartOfTryCatch = true

              case _ =>
                //BLOCK STATEMENT TRY
                tct.catchTask.taskMap += currentTask.locationIndex -> currentTask
                mainTask.taskMap.remove(loc.locationIndex)
                st.isPartOfTryCatch = true
                currentTask.isPartOfTryCatch = true
            }
          } //else : Contains test failed while checking for try catch.
      }
    }

    def retrieveCaseStatements(caseBodyLocations: MList[Location],
                               locationIter: LocationIterator,
                               currentState: CurrentState): Unit = {
      val loc = locationIter.next()
      if(loc.locationIndex == currentState.endOfLoop) {
        //Return: this is end of block.
        return
      }

      loc.statement match {
        case gs: GotoStatement =>
          val gotoTarget: Int = gs.targetLocation.locationIndex
          if(gotoTarget < loc.locationIndex) {
            addLocation(loc, caseBodyLocations )
            //todo Need to check this logic for end of loop. This is used to mark the end of default block.
            //If the goto target location is not within this caseBodyLocation, it marks the end of switch statement.
            if(!caseBodyLocations.exists(l => l.locationIndex == gotoTarget)) {
              currentState.endOfLoop = gotoTarget
            }
          } else {
            // Forward Jump in Switch Case Body. Adding location to Case Body
            // adding it to locations
            addLocation(loc, caseBodyLocations )
            retrieveCaseStatements(caseBodyLocations, locationIter, currentState)
          }
        case rs: ReturnStatement =>
          //Switch Statement Return
          currentState.endOfLoop = loc.locationIndex

        case _ =>
          addLocation(loc, caseBodyLocations)
          retrieveCaseStatements(caseBodyLocations, locationIter, currentState)
      }

      def addLocation(l: Location, loc: MList[Location]): Unit = {
        loc += l
      }
    }

    def retrieveLocation(locationIndex: Int, locationIterator: LocationIterator): Location = {
      locationIterator.getLocation(locationIndex)
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
        // If Backward jump, first get the elseBodyStatements.
        // Set location iter to next location after current if statement.
        // The mainIter is pointing to the previous if Statement.
        locationIter.setPos(originalLocation + 1)
        retrieveBackwardIfElseBodyLocations1(elseBodyLocations, originalLocation,
          ifStatement.targetLocation.locationIndex, locationIter, currentState)
      } else {
        // Set location iterator to the target location of If Jump.
        // This iterator will now be used to follow the If statements.
        locationIter.setPos(currentLocation)
        retrieveIfBodyLocations(ifBodyLocations, elseBodyLocations, originalLocation,
          ifStatement.targetLocation.locationIndex, locationIter, currentState)
        processIfElseStatements(ifBodyLocations, elseBodyLocations, originalLocation, locationIter, currentState)
      }

      // Reset the location iterator
      locationIter.setPos(originalLocation + 1)
      // IDENTIFY LOOP RESULT: If location = loc.locationIndex/loc.locationUri,
      // currentState.isLoop, Do...While: currentState.isPartOfBlock
      (ifBodyLocations, elseBodyLocations)
    }

    def checkDoWhileLoop(elseBodyLocations: MList[Location],
                         startLocation: Int,
                         originalTargetLocation: Int,
                         locationIter: LocationIterator,
                         currentState: CurrentState): Unit = {
      val locationsToAdd = locationIter.locations.filter (
        l => l.locationIndex >= originalTargetLocation && l.locationIndex <= startLocation )
      elseBodyLocations ++= locationsToAdd
      val currentLocation = locationIter.pos

      locationIter.setPos(elseBodyLocations.head.locationIndex)
      identifyDoLoop(startLocation, locationIter, currentState, elseBodyLocations)

      if(currentState.isLoop) {
        currentState.isPartOfBlock = true
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
      if (currentState.isPartOfBlock) {
        elseBodyLocations ++= elseBody
      } else {
        locationIter.setPos(startLocation +1 )
        //RETRIEVING BACKWARD IF...ELSE BODY LOCATIONS!
        retrieveBackwardIfElseBodyLocations(elseBodyLocations, startLocation,
          originalTargetLocation, locationIter, currentState)
      }
    }

    def retrieveBackwardIfElseBodyLocations(elseBodyLocations: MList[Location],
                                            startLocation: Int,
                                            originalTargetLocation: Int,
                                            locationIter: LocationIterator,
                                            currentState: CurrentState): Unit = {
      val loc = locationIter.next()

      loc.statement match {
        case gs: GotoStatement =>
          val gotoLocation = gs.targetLocation.locationIndex
          addLocation(loc, elseBodyLocations )
          currentState.endOfLoop = loc.locationIndex

          currentState.parentTask match {
            case Some(p) =>
              p  match {
                case wt: WhileTask =>
                  if(gotoLocation > wt.location.locationIndex ){
                    callIdentifyLoop(gs)
                  }

                case dwt: DoWhileTask =>
                  if(gotoLocation > dwt.location.locationIndex ){
                    callIdentifyLoop(gs)
                  }

                case it: IfElseTask =>
                  //If Else Task while retrieving Backward If Else Body Location.
                  if(gotoLocation > it.location.locationIndex ){
                    callIdentifyLoop(gs)
                  }

                case ct: CaseTask =>
                  if(gotoLocation > ct.location.locationIndex ){
                    callIdentifyLoop(gs)
                  }

                case _ =>
                  //println("Case is: " + p.getClass)
                  //println("SHOULD NOT BE HERE!!!")
                  //todo Check!
                  callIdentifyLoop(gs)
              }

            case None =>
              //No Parent Case... Need to check for do...while loops.
              //todo : Check if this condition is still necessary.
              addLocation(loc, elseBodyLocations )
              if(locationIter.hasNext) {
                retrieveBackwardIfElseBodyLocations(elseBodyLocations, startLocation,
                  originalTargetLocation, locationIter, currentState)
              }
          }

        case _ =>
          addLocation(loc, elseBodyLocations )
          if(locationIter.hasNext) {
            retrieveBackwardIfElseBodyLocations(elseBodyLocations, startLocation,
              originalTargetLocation, locationIter, currentState)
          }
      }

      def addLocation(l: Location, loc: MList[Location]): Unit = {
        loc += l
      }

      def callIdentifyLoop(gs: GotoStatement): Unit = {
        val currentLocation = locationIter.pos
        //Changing loop identification to top down.
        //locationIter.setPos(gs.targetLocation.locationIndex)
        locationIter.setPos(elseBodyLocations.head.locationIndex)
        identifyLoop(startLocation, locationIter, currentState, elseBodyLocations)
        locationIter.setPos(currentLocation)
      }
    }

    /**
      * Iteratively follow the if jump until a backward jump OR a return statement OR the end of file is encountered.
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
          //Adding goto in in block
          addLocation(loc, ifBodyLocations )

          // This is a forward jump case. Add the location and follow the jump statement.
          if(gotoLocation > loc.locationIndex) {
            addLocation(loc, ifBodyLocations )
            retrieveIfBodyLocations(ifBodyLocations, elseBodyLocations, startLocation,
              originalTargetLocation, locationIter, currentState)
          }
          // Backward jump, but after the initial If location
          // Add the location and all locations from initial If statement (@startLocation)
          // to the current goto target location(@gotoLocation)
          else if(gotoLocation < loc.locationIndex && gotoLocation > startLocation) {
            val addLocations = locationIter.locations.filter (
              l => l.locationIndex > startLocation && l.locationIndex < gotoLocation )
            elseBodyLocations ++= addLocations
            // In this case the code jumps to this position from if, so whole if block need not be included in else.
            //todo Handle this in a better way.
            currentState.hasEndOfElseMarker = locationIter.locations.filter (
              l => l.locationIndex > startLocation && l.locationIndex < originalTargetLocation )
                .exists(l=> l.statement.isInstanceOf[GotoStatement]
                    || l.statement.isInstanceOf[ReturnStatement]
                    || l.statement.isInstanceOf[ThrowStatement])
          }
          // Backward jump, but jumps even before the initial If location
          // Add the location and all locations from initial If location(@startLocation)
          // to the initial target location (originalTargetLocation)
          //This could be a loop
          else if (gotoLocation <= startLocation) {
            val addLocations = locationIter.locations.filter (
              l => l.locationIndex > startLocation && l.locationIndex < originalTargetLocation )
            elseBodyLocations ++= addLocations
          }

        case rs: ReturnStatement =>
          val addLocations = locationIter.locations.filter (
            l => l.locationIndex > startLocation && l.locationIndex < originalTargetLocation )
          // Location limit can be due to a If statement as well as it also has a goto part.
          // val locationLimit: Option[Location] = addLocations find (
          // al => al.statement.isInstanceOf[GotoStatement])
          removeLocationsFromIfBlock(addLocations)
          elseBodyLocations ++= addLocations

        case ts: ThrowStatement =>
          addLocation(loc, ifBodyLocations )
          /*ThrowStatement is also a location limit.
          So need to add elseBodyLocation same as in ReturnStatement*/
          val addLocations = locationIter.locations.filter (
            l => l.locationIndex > startLocation && l.locationIndex < originalTargetLocation )
          removeLocationsFromIfBlock(addLocations)
          elseBodyLocations ++= addLocations

        case _ =>
          addLocation(loc, ifBodyLocations)
          if(locationIter.hasNext) {
            retrieveIfBodyLocations(ifBodyLocations, elseBodyLocations,
              startLocation, originalTargetLocation, locationIter, currentState)
          } else {
            /* EOF reached without a goto or a return statement.
             So need to add elseBodyLocation same as in ReturnStatement */
            val addLocations = locationIter.locations.filter (
              l => l.locationIndex > startLocation && l.locationIndex < originalTargetLocation )
            removeLocationsFromIfBlock(addLocations)
            elseBodyLocations ++= addLocations
          }
      }

      def addLocation(l: Location, loc: MList[Location]): Unit = {
        loc += l
      }

      def removeLocationsFromIfBlock(addLocations: List[Location]): Unit = {
        val locationLimit: Option[Location] = addLocations find (al => al.statement.isInstanceOf[GotoStatement]
            || al.statement.isInstanceOf[IfStatement])

        // If there is a locationLimit -> Need to remove locations from @ifBodyLocations.
        locationLimit match {
          case Some(limit) =>
            addLocation(loc, ifBodyLocations)
            val targetLocation: Int = limit.statement match {
              case gs: GotoStatement => gs.targetLocation.locationIndex
              case is: IfStatement => is.targetLocation.locationIndex
              case _ => -1
            }
            if (targetLocation != -1) {
              val locationsToRemove = ifBodyLocations filter (l => l.locationIndex >= targetLocation)
              for (r <- locationsToRemove) {
                ifBodyLocations.remove(ifBodyLocations.indexOf(r))
              }
            }

          case None =>
        }
      }
    }

    def processIfElseStatements(ifBodyLocations: MList[Location],
                                elseBodyLocations: MList[Location],
                                startLocation: Int,
                                locationIter: LocationIterator,
                                currentState: CurrentState): Unit = {
      if (elseBodyLocations.nonEmpty) {
        //Filtering elseBodyLocations for endOfBlock marker (throwStatement)
        //        var hasEndOfBlockMarker: Boolean = false  //Added to currentState
        elseBodyLocations.find(l => l.statement.isInstanceOf[ThrowStatement] || l.statement.isInstanceOf[ReturnStatement]) match {
          case Some(loc) =>
            //Throw Statement in Else Body Locations.
            loc.statement match {
              case ts: ThrowStatement =>
                currentState.hasEndOfElseMarker = true
                val locationsToRemove = elseBodyLocations filter (l => l.locationIndex > loc.locationIndex)
                for (r <- locationsToRemove) {
                  elseBodyLocations.remove(elseBodyLocations.indexOf(r))
                }
              case rs: ReturnStatement =>
                currentState.hasEndOfElseMarker = true
                val locationsToRemove = elseBodyLocations filter (l => l.locationIndex >= loc.locationIndex)
                for (r <- locationsToRemove) {
                  elseBodyLocations.remove(elseBodyLocations.indexOf(r))
                }
              case _ =>
            }
          case None =>
          //No throw statement found.
        }

        //last statement may not be a goto. Find the last goto.
        val lastGoto = elseBodyLocations.lastIndexWhere(l => l.statement.isInstanceOf[GotoStatement])
        if (lastGoto != -1) {
          val gotoLocation: Location = elseBodyLocations(lastGoto)
          val gs: GotoStatement = gotoLocation.statement.asInstanceOf[GotoStatement]
          val locationsToRemove = elseBodyLocations filter (l => l.locationIndex > gotoLocation.locationIndex)

          for (r <- locationsToRemove) {
            elseBodyLocations.remove(elseBodyLocations.indexOf(r))
          }

          val currentLocation = locationIter.pos
          //Setting position to start of the else body for identifying the loop.
          locationIter.setPos(elseBodyLocations.head.locationIndex)
          identifyLoop(startLocation, locationIter, currentState, elseBodyLocations)
          if (!currentState.isLoop) {
            currentState.additionalState = IfState(startLocation)
          }

          //need to add removed statements to the parent block.
          currentState.parentTask match {
            case Some(p) =>
              if (currentState.isLoop) {
                ifBodyLocations.foreach {
                  l =>
                    //ADDING IF BODY IN LOOP TO PARENT >>>" + l.locationIndex + " :: " + l.locationUri)
                    p match {
                      case wt: WhileTask =>
                        wt.locationIterator.addLocation(l)

                      case dwt: DoWhileTask =>
                        dwt.locationIterator.addLocation(l)

                      case it: IfElseTask =>
                        if(it.ifTask.isActive){
                          it.ifTask.locationIterator.addLocation(l)
                        } else {
                          it.elseTask.locationIterator.addLocation(l)
                        }

                      case _ =>
                      //"PARENT IS : " + p.getClass + "!!!" + "::::" + l.locationIndex + " :: " + l.locationUri
                    }
                }
              }
              p match {
                case wt: WhileTask =>
                  locationsToRemove.foreach {
                    l =>
                      // ADDING Removed IN LOOP TO PARENT>>>
                      wt.locationIterator.addLocation(l)
                  }
                case _ => //PARENT IS NOT A WHILE LOOP!
              }

            case None =>
          }
          locationIter.setPos(currentLocation)
          currentState.parentTask match {
            case Some(p) =>
              p match {
                case dwt: DoWhileTask =>
                  if (ifBodyLocations.contains(dwt.location)) {
                    val locationsToRemove = ifBodyLocations filter (l => l.locationIndex >= dwt.location.locationIndex)
                    // Removing Location due to DO.. WHILE
                    for (r <- locationsToRemove) {
                      ifBodyLocations.remove(ifBodyLocations.indexOf(r))
                    }
                  }
                case _ => //PARENT IS NOT A DO..WHILE LOOP!
              }

            case None =>
          }
        }
        else if (!currentState.hasEndOfElseMarker){
          // If the else block has no goto statement then
          // all the if body blocks will also be executed in the pilar code.
          // Need to add them to elseBodyLocations. OR Remove all the if tasks to parent.
          ifBodyLocations.clear()
        }//else : This is not a loop as else block has no goto statement.
      }

      /* todo Check this logic: If no jump statement found in the if body location and parent is a do...while loop,
        this jump location is the initialisation part of do...while loop */
      if (ifBodyLocations.nonEmpty) {
        val locationLimit: Option[Location] = ifBodyLocations find (al => al.statement.isInstanceOf[GotoStatement]
            || al.statement.isInstanceOf[IfStatement])

        locationLimit match {
          case Some(limit) =>
            // If there is a locationLimit -> Need to remove locations from @ifBodyLocations.
            val targetLocation: Int = limit.statement match {
              case gs: GotoStatement => gs.targetLocation.locationIndex
              case is: IfStatement =>
                val isTargetLocation: Int = is.targetLocation.locationIndex

                // If the target location is within the elseBodyLocations, Remove the rest as well.
                if(isTargetLocation > startLocation && elseBodyLocations.nonEmpty) {
                  val locationsToRemoveFromElse = elseBodyLocations.filter(
                    l => l.locationIndex >= isTargetLocation)
                  for (r <- locationsToRemoveFromElse) {
                    elseBodyLocations.remove(elseBodyLocations.indexOf(r))
                  }
                }
                currentState.parentTask match {
                  case Some(p) =>
                    p match {
                      case mt: MainTask => -1
                      case _ => isTargetLocation
                    }
                  case None => -1
                }
              case ts: ThrowStatement =>
                limit.locationIndex
              case _ => -1
            }
            //todo need to verify this logic. Is used in cases : if{} while{}
            if (targetLocation != -1 && targetLocation <= limit.locationIndex){
              // IF FOLLOWED IMMEDIATELY BY WHILE
              // if (targetLocation <= limit.locationIndex){
              // val locationsToRemove = ifBodyLocations filter (l => l.locationIndex >= limit.locationIndex)
              // > Vs >=
              //              val locationsToRemove = ifBodyLocations filter (l => l.locationIndex >= limit.locationIndex)
              val locationsToRemove = ifBodyLocations filter (l => l.locationIndex > limit.locationIndex)
              for (r <- locationsToRemove) {
                ifBodyLocations.remove(ifBodyLocations.indexOf(r))
              }
            }

          case None =>
            //NO GOTO / IF STATEMENT IN IF BODY BLOCK
            currentState.parentTask match {
              case Some(p) =>
                p match {
                  case dwt: DoWhileTask =>
                    if (dwt.locationIterator.locations.containsSlice(ifBodyLocations)) {
                      //parent do...while loop contains all the ifbodyLocations.
                      val locationsToRemove = ifBodyLocations
                      for (r <- locationsToRemove) {
                        ifBodyLocations.remove(ifBodyLocations.indexOf(r))
                      }
                    }
                  case _ => //"PARENT IS NOT A DO..WHILE LOOP!"
                }
              case None =>
            }
        }
      }
    }

    def identifyDoLoop(startLocation: Int,
                       locationIter: LocationIterator,
                       currentState: CurrentState,
                       elseBodyLocations: MList[Location],
                       locationsToAdd: MSet[Location] = msetEmpty): Unit = {
      val loc = locationIter.next()

      loc.statement match {
        case gs: GotoStatement =>
          //todo check if we need this || part in gotoStatement
          // Setting endOfLoop as the original If Statement from DoWhileTask
          // currentState.endOfLoop = loc.locationIndex
          if(gs.targetLocation.locationIndex < loc.locationIndex
              || locationIter.locations.exists(l => l.locationIndex == gs.targetLocation.locationIndex)) {
            locationIter.setPos(gs.targetLocation.locationIndex)
          }
        case rs: ReturnStatement =>
          //Not a Do loop : Return
          return

        case is: IfStatement =>
          if(loc.locationIndex == startLocation) {
            currentState.isLoop = true
            //Setup Loop State:
            currentState.additionalState = LoopState(startLocation)
            elseBodyLocations ++= locationsToAdd
            currentState.loopInitialisers ++= locationsToAdd
            return
          }
          else if (currentState.parentTask.isDefined) {
            currentState.parentTask match {
              case Some(p) =>
                if(is.targetLocation.locationIndex > loc.locationIndex
                    && locationIter.locations.exists(l => l.locationIndex == is.targetLocation.locationIndex)) {
                  locationIter.setPos(is.targetLocation.locationIndex)
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

        case _ =>
          if(!elseBodyLocations.contains(loc)) {
            locationsToAdd += loc
            //Resetting visited.
            currentState.resetVisitedLocation(loc.locationIndex)
          }
      }
      //Moved This outside so that this happens everytime now that we are checking if body from top to bottom.
      if(locationIter.hasNext) {
        identifyDoLoop(startLocation, locationIter,
          currentState, elseBodyLocations, locationsToAdd)
      }
    }

    /** Follow the goto to check if the original IfStatement
      * OR if the parent is a loop, then see if parents if can be reached or not.
      * If it can be reached, then it is a loop.
      */
    def identifyLoop (startLocation: Int,
                      locationIter: LocationIterator,
                      currentState: CurrentState,
                      elseBodyLocations: MList[Location],
                      locationsToAdd: MSet[Location] = msetEmpty): Unit = {
      val loc = locationIter.next()

      loc.statement match {
        case gs: GotoStatement =>
          //todo check if we need this || part in gotoStatement
          currentState.endOfLoop = loc.locationIndex
          if(gs.targetLocation.locationIndex < loc.locationIndex
              || locationIter.locations.exists(l => l.locationIndex == gs.targetLocation.locationIndex)) {
            locationIter.setPos(gs.targetLocation.locationIndex)
          }

        case rs: ReturnStatement =>
          //Not a loop : Return
          return

        case is: IfStatement =>
          if(loc.locationIndex == startLocation){
            currentState.isLoop = true
            //Setup Loop State:
            currentState.additionalState = LoopState(startLocation)
            elseBodyLocations ++= locationsToAdd
            currentState.loopInitialisers ++= locationsToAdd
            return
          } else {
            if((is.targetLocation.locationIndex < startLocation
                && currentState.endOfLoop != -1)
                || (is.targetLocation.locationIndex > loc.locationIndex )) {
              locationIter.setPos(is.targetLocation.locationIndex)
            }
          }

        case _ =>
          if(!elseBodyLocations.contains(loc)) {
            locationsToAdd += loc
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
      if(currentState.getVisitedCount(loc.locationIndex) > 0) {
        //this location has already been visited: Return
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

        case ts: ThrowStatement =>
          bodyStatements += ((loc.locationIndex, visitThrowStatement(ts)))

        case es: EmptyStatement =>
        case gs: GotoStatement =>
        case ss: SwitchStatement => //todo check!!!

        case _ =>
          throw new Jawa2JavaTranslateException("No matching Statement Found."
              + loc.locationIndex + " :: " + loc.locationUri + " :: " + statement.getClass)
      }
      //todo pick one??
      //    mainIter.visitLocation(loc.locationIndex)
      currentState.visitLocation(loc.locationIndex)
    }
  }

  trait Task {
    var isResolved: Boolean = false
    var isLevel1: Boolean = true // use this to filter nested lines within some other block.
    var isPartOfBlock: Boolean = false
    var isPartOfTryCatch: Boolean = false
    var locationIndex: Int = _
    def resolve(bodyStatements: MList[(Int, ST)]): Unit
  }

  trait BlockTask extends Task {
    val taskMap: mutable.LinkedHashMap[Int, Task] = mlinkedMapEmpty
    val addedLocations: MList[Int] = mlistEmpty
    val level: Int
    var endOfBlock: Int = _

    def identifyTasks(): Unit

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

    def addVisitedLocations(locationsToAdd: MMap[Int, Int]): Unit = {
      visitedLocations ++= locationsToAdd
    }

    def setIsPartOfBlock(locationIndex: Int): Unit = {
      taskMap(locationIndex).isPartOfBlock = true
    }

    def checkTask(locationIndex: Int): Boolean = {
      taskMap.exists{case(loc, _)=> loc == locationIndex}
    }
  }

  case class MainTask(locations: IList[Location],
                      thisParam: Option[Param],
                      isConstructor: Boolean,
                      catchClauses: IList[CatchClause] = ilistEmpty) extends BlockTask with TaskHelper {

    val level = 0
    val resolvedTaskMap: IMap[Int, ST] = imapEmpty
    val tryCatchTaskMap: mutable.LinkedHashMap[Int, TryCatchTask] = mlinkedMapEmpty
    var endOfTryCatch: Int = _

    val locationIterator: LocationIterator = LocationIterator(Left(locations))

    val currentState = CurrentState(isConstructor = isConstructor,
      isIfStatement = false, isElseIfStatement = false, isElseStatement = false, targetLocation = null)

    val imports: MSet[JawaType] = msetEmpty

    def identifyTryCatchTasks(): Unit = {
      //Identifying Try Catch Tasks.
      catchClauses.foreach{
        c=>
          val tryBodyLocations: MList[Location] = mlistEmpty
          val catchBodyLocations: MList[Location] = mlistEmpty
          endOfTryCatch = c.range.toLocation.locationIndex
          locationIterator.setPos(c.targetLocation.locationIndex)
          retrieveCatchBodyLocations(c, catchBodyLocations)

          locationIterator.setPos(c.range.fromLocation.locationIndex)
          retrieveTryBodyLocations(c, tryBodyLocations)

          val tryCatchTask: TryCatchTask = TryCatchTask(c, tryBodyLocations, catchBodyLocations, this,  this)
          tryCatchTaskMap += tryCatchTask.tryTask.locationIndex -> tryCatchTask
      }
      locationIterator.setPos(0)
    }

    def retrieveCatchBodyLocations(c: CatchClause, catchBodyLocations: MList[Location]): Unit = {
      val loc = locationIterator.next()
      loc.statement match {
        case gs: GotoStatement =>
          endOfTryCatch = gs.targetLocation.locationIndex
        case rs: ReturnStatement =>
          if(endOfTryCatch > loc.locationIndex) {
            endOfTryCatch = loc.locationIndex
          }
        case _ =>
          catchBodyLocations += loc
          if(locationIterator.hasNext) {
            retrieveCatchBodyLocations(c, catchBodyLocations)
          }
      }
    }

    def retrieveTryBodyLocations(c: CatchClause, tryBodyLocations: MList[Location]): Unit = {
      val loc = locationIterator.next()
      if(loc.locationIndex != endOfTryCatch && loc.locationIndex != c.targetLocation.locationIndex){
        tryBodyLocations += loc
        if(locationIterator.hasNext) {
          retrieveTryBodyLocations(c, tryBodyLocations)
        }
      }
    }

    def identifyTasks(): Unit = {
      if (locationIterator.hasNext) {
        val loc: Location = locationIterator.next()
        val currentTask: Task = identifyTask(loc, locationIterator, this, this, this)
        taskMap += loc.locationIndex -> currentTask

        currentTask match {
          case bt: BlockTask =>
            bt.identifyTasks()
            locationIterator.setPos(bt.endOfBlock)
          case _ =>
        }
        identifyTasks()
      }
    }

    def taskExists(locationIndex: Int): Boolean = taskMap.contains(locationIndex)

    override def checkLocation(locationIndex: Int): Boolean = locations.exists(loc=> loc.locationIndex == locationIndex)

    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      taskMap foreach {
        case (loc, task) =>
          if(getVisitedCount(loc) == 0 && !task.isPartOfBlock && !task.isPartOfTryCatch) {
            task.resolve(bodyStatements)
            visitLocation(loc)
          }
      }
    }

    def resolveTryCatchBlocks(bodyStatements: MList[(Int, ST)]): Unit = {
      tryCatchTaskMap foreach {
        case (loc, task) =>
          if(getVisitedCount(loc) == 0 && !task.isPartOfBlock) {
            task.resolve(bodyStatements)
            visitLocation(loc)
          }
      }
    }
  }

  case class SimpleTask(location: Location,
                        mainTask: MainTask) extends Task with TaskHelper {
    locationIndex = location.locationIndex
    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      visitSimpleStatement(mainTask.imports, bodyStatements, mainTask.isConstructor,
        mainTask.thisParam, location, location.statement, CurrentState())
    }
  }

  case class NoneTask() extends Task {
    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      //This is a None Task.
    }
  }

  case class IfElseTask(location: Location,
                        ifBodyLocations: MList[Location],
                        elseBodyLocations: MList[Location],
                        mainTask: MainTask,
                        parentTask: BlockTask) extends BlockTask with TaskHelper {
    locationIndex = location.locationIndex
    val level: Int = parentTask.level + 1
    val ifTask: IfTask = IfTask(location, ifBodyLocations, mainTask, parentTask, this)
    val elseTask: IfTask = IfTask(location, elseBodyLocations, mainTask, parentTask, this, isElse = true)

    def identifyTasks(): Unit = {
      ifTask.isActive = true
      ifTask.identifyTasks()
      ifTask.isActive = false
      elseTask.isActive = true
      elseTask.identifyTasks()
      elseTask.isActive = false

      taskMap ++= ifTask.taskMap
      taskMap ++= elseTask.taskMap
    }

    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      ifTask.resolve(bodyStatements)
      elseTask.resolve(bodyStatements)
    }

    override def setIsPartOfBlock(locationIndex: Int): Unit = {
      if(ifTask.taskMap.contains(locationIndex)){
        ifTask.taskMap(locationIndex).isPartOfBlock = true
      } else {
        elseTask.taskMap(locationIndex).isPartOfBlock = true
      }
    }

    // Check for a task within the active if/else task.
    override def checkTask(locationIndex: Int): Boolean = {
      (ifTask.isActive && ifTask.taskMap.contains(locationIndex)) ||
          (elseTask.isActive && elseTask.taskMap.contains(locationIndex))
    }

    override def addVisitedLocations(locationsToAdd: MMap[Int, Int]): Unit = {
      if(ifTask.isActive) {
        ifTask.visitedLocations ++= locationsToAdd
      } else if (elseTask.isActive){
        elseTask.visitedLocations ++= locationsToAdd
      }
    }
  }

  case class IfTask(location: Location,
                    ifBodyLocations: MList[Location],
                    mainTask: MainTask,
                    parentTask: BlockTask,
                    siblingOfTask: IfElseTask,
                    isElse: Boolean = false) extends BlockTask with TaskHelper {

    var isActive: Boolean = false
    val level: Int = parentTask.level + 1
    val ifStatement: IfStatement = location.statement.asInstanceOf[IfStatement]
    val locationIterator: LocationIterator = LocationIterator(Right(ifBodyLocations))

    def identifyTasks(): Unit = {
      if (locationIterator.hasNext) {

        val loc: Location = locationIterator.next()
        val currentTask: Task = identifyTask(loc, locationIterator, mainTask,
          siblingOfTask, parentTask)
        taskMap += loc.locationIndex -> currentTask

        currentTask match {
          case bt: BlockTask =>
            bt.identifyTasks()
            // Setting next iterator location based on endOfBlock in inner Block Task.
            // In some cases, the end of inner block is not a part of the parent task.
            // Hence, looking for a location higher than that.
            val endOfBlock: Int = bt.endOfBlock
            if(checkLocation(endOfBlock)) {
              locationIterator.setPos(endOfBlock)
            } else {
              locationIterator.locations.find(l => l.locationIndex >= endOfBlock) match {
                case Some(higherLocation) => locationIterator.setPos(higherLocation.locationIndex)
                case None =>
              }
            }
          case _ =>
        }
        identifyTasks()
      }
    }

    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      val ifBodyStatements: MList[(Int, ST)] = mlistEmpty
      isActive = true
      taskMap foreach {
        case (loc, task) =>
          if(getVisitedCount(loc) == 0 && !task.isPartOfBlock) {
            task.resolve(ifBodyStatements)
            visitLocation(loc)
          } // else location already visited.
      }
      parentTask.addVisitedLocations(visitedLocations)

      isActive = false
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

    override def checkLocation(locationIndex: Int): Boolean = {
      ifBodyLocations.exists(loc=> loc.locationIndex == locationIndex)
    }
  }

  case class WhileTask(location: Location,
                       loopBodyLocations: MList[Location],
                       mainTask: MainTask,
                       parentTask: BlockTask) extends BlockTask with TaskHelper {
    locationIndex = location.locationIndex
    val locationIterator: LocationIterator = LocationIterator(Right(loopBodyLocations))
    val ifStatement: IfStatement = location.statement.asInstanceOf[IfStatement]
    val startOfLoop: Int = location.locationIndex
    val loopInitialisers: MSet[Location] = msetEmpty
    val level = parentTask.level + 1

    def identifyTasks(): Unit = {
      if (locationIterator.hasNext) {
        val loc: Location = locationIterator.next()
        val currentTask: Task = identifyTask(loc, locationIterator, mainTask, this, parentTask)
        taskMap += loc.locationIndex -> currentTask

        if (parentTask.checkTask(loc.locationIndex)) {
          parentTask.setIsPartOfBlock(loc.locationIndex)
        }

        currentTask match {
          case bt: BlockTask =>
            bt.identifyTasks()
            // Setting next iterator location based on endOfBlock in inner Block Task.
            // In some cases, the end of inner block is not a part of the parent task.
            // Hence, looking for a location higher than that.
            // In some cases this is the last statement and need not set it to any other location.
            val endOfBlock: Int = bt.endOfBlock
            if(checkLocation(endOfBlock)) {
              locationIterator.setPos(endOfBlock)
            } else {
              locationIterator.locations.find(l => l.locationIndex >= endOfBlock) match {
                case Some(higherLocation) => locationIterator.setPos(higherLocation.locationIndex)
                case None =>
              }
            }
          case _ =>
        }
        identifyTasks()
      }
    }

    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      val initialiserStatements: MList[(Int, ST)] = mlistEmpty
      val loopBodyStatements: MList[(Int, ST)] = mlistEmpty

      taskMap foreach {
        case (loc, task) =>
          if(getVisitedCount(loc) == 0 && loc <= endOfBlock && !task.isPartOfBlock) {
            task.resolve(loopBodyStatements)
            if( loopInitialisers.exists(l => l.locationIndex == loc)) {
              task.resolve(initialiserStatements)
            }
            visitLocation(loc)
          }
      }
      parentTask.addVisitedLocations(visitedLocations)
      val loopTemplate: ST = template.getInstanceOf("WhileLoop")
      initialiserStatements map {
        st =>
          loopTemplate.add("initialisers", st._2)
      }
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
      loopBodyLocations.exists(loc=> loc.locationIndex == locationIndex)
    }
  }

  case class DoWhileTask(location: Location,
                         loopBodyLocations: MList[Location],
                         mainTask: MainTask,
                         parentTask: BlockTask) extends BlockTask with TaskHelper {
    locationIndex = location.locationIndex

    val level = parentTask.level + 1
    val ifStatement: IfStatement = location.statement.asInstanceOf[IfStatement]
    val startOfLoop: Int = ifStatement.targetLocation.locationIndex
    val locationIterator: LocationIterator = LocationIterator(Right(loopBodyLocations))
    // todo is it required for do while loop???
    // val loopInitialisers: MSet[Location] = msetEmpty

    def identifyTasks(): Unit = {
      //Identifying Do While Block Tasks
      /* todo mark others isLevel1 = false. Put task in the first line of loop.
         Incorporate all loop statements in this task. */
      if (locationIterator.hasNext) {
        val loc: Location = locationIterator.next()
        if(location.locationIndex != loc.locationIndex ) {
          val currentTask: Task = identifyTask(loc, locationIterator, mainTask, this, parentTask)
          taskMap += loc.locationIndex -> currentTask
          if (parentTask.checkTask(loc.locationIndex)) {
            parentTask.setIsPartOfBlock(loc.locationIndex)
          }

          currentTask match {
            case bt: BlockTask =>
              bt.identifyTasks()
              // Setting next iterator location based on endOfBlock in inner Block Task.
              // In some cases, the end of inner block is not a part of the parent task.
              // Hence, looking for a location higher than that.
              // In some cases this is the last statement and need not set it to anyother location.
              val endOfBlock: Int = bt.endOfBlock
              if(checkLocation(endOfBlock)) {
                locationIterator.setPos(endOfBlock)
              } else {
                locationIterator.locations.find(l => l.locationIndex >= endOfBlock) match {
                  case Some(higherLocation) => locationIterator.setPos(higherLocation.locationIndex)
                  case None =>
                }
              }
            case _ =>
          }
          identifyTasks()
        }
      }
    }

    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      val loopBodyStatements: MList[(Int, ST)] = mlistEmpty
      taskMap.toSeq.sortBy(_._1) foreach {
        case (loc, task) =>
          if(getVisitedCount(loc) == 0 && loc <= endOfBlock && !task.isPartOfBlock) {
            task.resolve(loopBodyStatements)
            visitLocation(loc)
          }
      }
      parentTask.addVisitedLocations(visitedLocations)

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

    override def checkLocation(locationIndex: Int): Boolean = {
      loopBodyLocations.exists(loc=> loc.locationIndex == locationIndex)
    }
  }

  case class SwitchTask(location: Location,
                        switchStatement: SwitchStatement,
                        switchCases: MLinkedMap[String, MList[Location]],
                        defaultCase: Option[MList[Location]],
                        mainTask: MainTask,
                        parentTask: BlockTask) extends BlockTask with TaskHelper {
    locationIndex = location.locationIndex
    override val level: Int = parentTask.level + 1
    var endOfDefault: Int = _
    val caseTaskList = createCaseTasks()

    def createCaseTasks(): MList[CaseTask] = {
      val caseTaskList: MList[CaseTask] = mlistEmpty
      var caseCount: Int = 0

      switchCases foreach {
        case(c, locations) =>
          caseTaskList += CaseTask(location, c, caseCount, locations, mainTask, parentTask )
          caseCount += 1
      }

      defaultCase match {
        case Some(d) =>
          caseTaskList += CaseTask(location, "default", caseCount, d,
            mainTask, parentTask, isDefault = true)

        case None =>
      }
      caseTaskList
    }

    override def identifyTasks(): Unit = {
      caseTaskList foreach {
        task =>
          task.identifyTasks()
      }
    }

    override def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      val switchTemplate: ST = template.getInstanceOf("SwitchStatement")
      switchTemplate.add("exp", switchStatement.condition.varName)

      caseTaskList foreach {
        caseTask =>
          val caseBodyStatements: MList[(Int, ST)] = mlistEmpty
          caseTask.resolve(caseBodyStatements)
          val caseTemplate: ST = caseTask.isDefault match {
            case true =>
              template.getInstanceOf("DefaultCase")
            case false =>
              template.getInstanceOf ("SwitchCase").add ("val", caseTask.switchCase)
          }

          val caseBodyTemplate = template.getInstanceOf("Body")
          caseBodyStatements.sortBy(_._1).map {
            st =>
              caseBodyTemplate.add("statements", st._2)
          }
          caseTemplate.add("body", caseBodyTemplate)

          switchTemplate.add("cases", caseTemplate)
      }
      bodyStatements += ((location.locationIndex, switchTemplate))
    }
  }

  case class CaseTask(location: Location,
                      switchCase: String,
                      caseCount: Int,
                      caseBodyLocations: MList[Location],
                      mainTask: MainTask,
                      parentTask: BlockTask,
                      isDefault: Boolean = false) extends BlockTask with TaskHelper {
    locationIndex = location.locationIndex

    val level = parentTask.level + 1
    val locationIterator: LocationIterator = LocationIterator(Right(caseBodyLocations))

    def identifyTasks(): Unit = {
      if (locationIterator.hasNext) {

        val loc: Location = locationIterator.next()
        val currentTask: Task = identifyTask(loc, locationIterator, mainTask, this, parentTask)
        taskMap += loc.locationIndex -> currentTask

        currentTask match {
          case bt: BlockTask =>
            bt.identifyTasks()
            // Setting next iterator location based on endOfBlock in inner Block Task.
            // In some cases, the end of inner block is not a part of the parent task.
            // Hence, looking for a location higher than that.
            // In some cases this is the last statement and need not set it to anyother location.
            val endOfBlock: Int = bt.endOfBlock
            if(checkLocation(endOfBlock)) {
              locationIterator.setPos(endOfBlock)
            } else {
              locationIterator.locations.find(l => l.locationIndex >= endOfBlock) match {
                case Some(higherLocation) => locationIterator.setPos(higherLocation.locationIndex)
                case None =>
              }
            }
          case _ =>
        }
        identifyTasks()
      }
    }

    def resolve(caseBodyStatements: MList[(Int, ST)]): Unit = {
      taskMap foreach {
        case (loc, task) =>
          if(getVisitedCount(loc) == 0 && !task.isPartOfBlock) {
            task.resolve(caseBodyStatements)
            visitLocation(loc)
          } // Already Visited.
      }
      parentTask.addVisitedLocations(visitedLocations)
    }

    override def checkLocation(locationIndex: Int): Boolean = {
      val ret: Boolean = caseBodyLocations.exists(loc=> loc.locationIndex == locationIndex)
      ret
    }
  }

  case class TryCatchTask(catchClause: CatchClause,
                          tryBodyLocations: MList[Location],
                          catchBodyLocations: MList[Location],
                          mainTask: MainTask,
                          parentTask: BlockTask) extends BlockTask with TaskHelper {
    locationIndex = catchClause.range.fromLocation.locationIndex
    val level: Int = parentTask.level + 1
    val tryTask: TryTask = TryTask(catchClause.range.fromLocation, tryBodyLocations, mainTask, parentTask)
    val catchTask: CatchTask = CatchTask(catchClause.targetLocation, catchClause.typ.typ.canonicalName,
      catchBodyLocations, mainTask, parentTask)

    def identifyTasks(): Unit = {
      // Tasks are already identified. Only wrapping them in Try Catch Blocks.
    }

    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      val tryCatchTemplate: ST = template.getInstanceOf("TryCatchStatement")
      /** Resolve Try Task */
      val tryBodyStatements: MList[(Int, ST)] = mlistEmpty
      tryTask.resolve(tryBodyStatements)

      val tryTemplate: ST = template.getInstanceOf("TryStatement")
      val tryBodyTemplate: ST = template.getInstanceOf("Body")
      tryBodyStatements.sortBy(_._1) map {
        st =>
          tryBodyTemplate.add("statements", st._2)
      }
      tryTemplate.add("body", tryBodyTemplate)
      tryCatchTemplate.add("try", tryTemplate)

      /** Resolve Catch Task */
      val catchBodyStatements: MList[(Int, ST)] = mlistEmpty
      val catchTemplate: ST = template.getInstanceOf("CatchStatement")

      catchTask.resolve(catchBodyStatements)

      val catchBodyTemplate: ST = template.getInstanceOf("Body")
      val exceptionClause: ST = catchBodyStatements.head._2
      catchTemplate.add("exception",
        s"${catchTask.catchType} ${exceptionClause.getAttribute("lhs").asInstanceOf[ST].render}" )

      catchBodyStatements.remove(catchTask.locationIndex)    //Remove the exception i.e. the head.
      catchBodyStatements.sortBy(_._1) map {
        st =>
          catchBodyTemplate.add("statements", st._2)
      }
      catchTemplate.add("body", catchBodyTemplate)
      tryCatchTemplate.add("catch", catchTemplate)

      bodyStatements += ((locationIndex, tryCatchTemplate))
    }
  }

  case class TryTask(location: LocationSymbol,
                     tryBodyLocations: MList[Location],
                     mainTask: MainTask,
                     parentTask: BlockTask) extends BlockTask with TaskHelper {
    val level: Int = 1
    def identifyTasks(): Unit = {
      // Tasks are already identified. Only wrapping them in Try Catch Blocks.
    }

    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      taskMap foreach {
        case (loc, task) =>
          if(mainTask.getVisitedCount(loc) == 0 && !task.isPartOfBlock) {
            task.resolve(bodyStatements)
            task.isPartOfTryCatch = true
            task match  {
              case bt: BlockTask =>
                mainTask.visitedLocations ++= bt.visitedLocations
              case _ =>
            }
            mainTask.visitLocation(loc)
          }
      }
    }
  }

  case class CatchTask(location: LocationSymbol,
                       catchType: String,
                       catchBodyLocations: MList[Location],
                       mainTask: MainTask,
                       parentTask: BlockTask) extends BlockTask with TaskHelper {
    val level: Int = 1
    def identifyTasks(): Unit = {
      // Tasks are already identified. Only wrapping them in Try Catch Blocks.
    }

    def resolve(bodyStatements: MList[(Int, ST)]): Unit = {
      taskMap foreach {
        case (loc, task) =>
          if(mainTask.getVisitedCount(loc) == 0 && !task.isPartOfBlock) {
            task.resolve(bodyStatements)
            task.isPartOfTryCatch = true
            task match  {
              case bt: BlockTask =>
                mainTask.visitedLocations ++= bt.visitedLocations
              case _ =>
            }
            mainTask.visitLocation(loc)
          }
      }
    }
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
      // pos is incremented automatically hence points to the next location, which might or might not exist.
      // Handle for end of iterator.
      // throw exception when the desired location is not the end of iterator and still not found
      if(pos == -1 && locations.nonEmpty) {
        val maxLocation: Int = locations.maxBy( l=> l.locationIndex).locationIndex + 1
        if(maxLocation == locationIndex) {
          pos = maxLocation
        } else {
          throw new Jawa2JavaTranslateException("Error Setting location. Target Location "
              + locationIndex +" not found within iterator." )
        }
      }
    }

    def getLocation(locationIndex: Int): Location = {
      val p: Int = locations.indexWhere(l=> l.locationIndex == locationIndex)
      if(p == -1) {
        throw new Jawa2JavaTranslateException("Error Getting location. Target Location "
            + locationIndex +" not found within iterator." )
      } else {
        locations(p)
      }
    }

    def addLocation(location: Location): Unit = {
      locations = locations :+ location
    }

    def removeLocation(location: Location): Unit = {
      locations = locations filterNot(l => l == location)
    }
  }

  //todo Clean up
  case class CurrentState(isConstructor: Boolean = false,
                          var isIfStatement: Boolean = false,
                          var isElseIfStatement: Boolean = false,
                          var isElseStatement: Boolean = false,
                          var isLoop: Boolean = false,
                          var isPartOfBlock: Boolean = false,
                          var hasEndOfElseMarker: Boolean = false,
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

    val sortedImports: List[JawaType] =
      imports.toList filterNot(_.baseTyp == cid.typ.name) sortWith((x, y) => x.baseTyp < y.baseTyp)

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

    //    println("\n$$$$$$$$$$$$$$$$$$$$$$$$$$$\nCURRENT METHOD IS:" + md.name +"\n$$$$$$$$$$$$$$$$$$$$$$$$$$$ ")
    if(isConstructor) {
      methodTemplate.add("accessFlag",
        AccessFlag.toString(AccessFlag.getAccessFlags(md.accessModifier)).replace("constructor", "").trim)
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

        val mainTask = MainTask(resolvedBody.locations, md.thisParam, isConstructor, resolvedBody.catchClauses)
        mainTask.identifyTryCatchTasks()
        mainTask.identifyTasks()
        mainTask.resolveTryCatchBlocks(bodyStatements)
        mainTask.resolve(bodyStatements)

        imports ++= mainTask.imports

        val bodyTemplate = template.getInstanceOf("Body")
        bodyStatements.sortBy(_._1).map {
          st =>
            bodyTemplate.add("statements", st._2)
        }
        methodTemplate.add("body", bodyTemplate)

      case UnresolvedBody(bodytokens) =>
    }
    methodTemplate
  }

  private def visitAssignmentStatement(as: AssignmentStatement,
                                       thisParam: Option[Param],
                                       imports: MSet[JawaType]): ST = {
    val assignmentTemplate = template.getInstanceOf("AssignmentStatement")
    val lhs: ST = visitExpressionLHS(as.lhs, thisParam, imports)
    val rhs: ST = visitExpressionRHS(as.rhs, thisParam, imports)

    assignmentTemplate.add("lhs", lhs)
    assignmentTemplate.add("rhs", rhs)

    assignmentTemplate
  }

  private def visitExpressionLHS(lhs: Expression with LHS,
                                 thisParam: Option[Param],
                                 imports: MSet[JawaType]): ST = {
    lhs match {
      case ne: NameExpression =>
        visitNameExpression(ne, imports)

      //todo only tested for RHS
      case ae: AccessExpression =>
        visitAccessExpression(ae, thisParam, imports)

      case ie: IndexingExpression =>
        visitIndexingExpression(ie)

      case _ => throw new Jawa2JavaTranslateException("No matching LHS expression on line: "
          + lhs.pos.line + ":" + lhs.pos.column )
    }
  }

  private def visitExpressionRHS(rhs: Expression with RHS,
                                 thisParam:Option[Param],
                                 imports: MSet[JawaType]): ST = {
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

      case ee: ExceptionExpression =>
        visitExceptionExpression(ee)

      case _ =>
        throw new Jawa2JavaTranslateException("No matching RHS expression on line: "
            + rhs.pos.line + ":" + rhs.pos.column )
    }
  }

  private def visitNameExpression(ne: NameExpression,
                                  imports: MSet[JawaType]): ST = {
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

      case _ => throw new Jawa2JavaTranslateException("No matching Literal Expression: "
          + le.pos.line + ":" + le.pos.column )
    }
  }

  private def visitAccessExpression(ae: AccessExpression,
                                    thisParam: Option[Param],
                                    imports: MSet[JawaType]): ST = {
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

  private def visitCastExpression(ce: CastExpression,
                                  imports: MSet[JawaType]): ST = {
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

  private def visitInstanceofExpression(insof: InstanceofExpression,
                                        imports: MSet[JawaType]): ST = {
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
      case _ => throw new Jawa2JavaTranslateException("Unidentified cmp expression."
          + cmp.pos.line + ":" + cmp.pos.column )
    }
    cmpTemplate.add("left", cmp.var1Symbol.varName)
    cmpTemplate.add("op", op)
    cmpTemplate.add("right", cmp.var2Symbol.varName)
    cmpTemplate
  }

  private def visitExceptionExpression(ee: ExceptionExpression): ST = {
    val exceptionTemplate = template.getInstanceOf("ExceptionExpression")
    exceptionTemplate.add("exception", ee.exception.rawtext)
    exceptionTemplate
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

  private def visitThrowStatement(ts: ThrowStatement): ST = {
    val throwTemplate = template.getInstanceOf("ThrowStatement")
    throwTemplate.add("exception", ts.varSymbol.varName)
    throwTemplate
  }

  private def visitCallStatement(cs: CallStatement,
                                 imports: MSet[JawaType]): ST = {
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