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

import java.io.File

import org.argus.jawa.core.{MsgLevel, PrintReporter}
import org.argus.jawa.core.io.{FgSourceFile, PlainFile}
import org.scalatest._

/**
  * @author <a href="mailto:fgwei521@gmail.com">Fengguo Wei</a>
  */
class Jawa2JavaTest extends FlatSpec with ShouldMatchers {

  implicit def file2TestFile(s: FgSourceFile): TestFile =
    new TestFile(s)

  new FgSourceFile(new PlainFile(new File("src/test/resources/simple/RecordDecl.pilar"))) produceJavaClass
  """package com.fgwei;
    |
    |
    |public class RecordDecl {
    |}
  """.stripMargin.trim

  class TestFile(s: FgSourceFile) {

    def produceJavaClass(classStr: String)() {
      val reporter = new PrintReporter(MsgLevel.INFO)
      val translator = new Jawa2Java(reporter)
      val javaClass = translator.translate(Right(s)).values.mkString("")
      require(javaClass == classStr)
    }

  }
}
