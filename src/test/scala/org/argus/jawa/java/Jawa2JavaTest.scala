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

import org.argus.jawa.core.DefaultReporter
import org.argus.jawa.core.io.{FgSourceFile, PlainFile}
import org.scalatest._

/**
  * @author <a href="mailto:fgwei521@gmail.com">Fengguo Wei</a>
  */
class Jawa2JavaTest extends FlatSpec with ShouldMatchers {

  implicit def file2TestFile(s: FgSourceFile): TestFile =
    new TestFile(s)

/*  new FgSourceFile(new PlainFile(new File("src/test/resources/simple/RecordDecl.pilar"))) produceJavaClass
  """package com.fgwei;
    |
    |
    |public class RecordDecl {
    |}
  """.stripMargin.trim*/

  new FgSourceFile(new PlainFile(new File("src/test/resources/simple/RecordDecl.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |
      |public class RecordDecl {
      |  private int i1;
      |  public static int main() {
      |
      |
      |  }
      |}
    """.stripMargin.trim

  new FgSourceFile(new PlainFile(new File("src/test/resources/simple/RecordDecl1.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |
      |public class RecordDecl {
      |  private int i1;
      |  public static int main() {
      |     int i2;
      |
      |
      |  }
      |}
    """.stripMargin.trim

  new FgSourceFile(new PlainFile(new File("src/test/resources/simple/RecordDecl2.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.io.File;
      |import java.lang.String;
      |import java.util.ArrayList;
      |
      |public class RecordDecl {
      |  private int i1;
      |  public static int main(String param1, ArrayList arr_param_1) {
      |     int i2;
      |     String s1;
      |     File file1;
      |
      |  }
      |}
    """.stripMargin.trim

  new FgSourceFile(new PlainFile(new File("src/test/resources/simple/RecordDecl3_Locations.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.io.File;
      |import java.lang.String;
      |import java.util.ArrayList;
      |
      |public class RecordDecl {
      |  private int i1;
      |   int i3;
      |   int[] int_arr1;
      |
      |  public static int main(String param1, ArrayList arr_param_1) {
      |    int i2;
      |    String s1;
      |    String s2;
      |    File file1;
      |    float f1;
      |    String[] String_arr1;
      |    int[] int_arr2;
      |
      |    s1 = "testing";
      |    s2 = new String();
      |    i2 = 55;
      |    f1 = 1.5;
      |    return i2;
      |  }
      |}
    """.stripMargin.trim

//  new FgSourceFile(new PlainFile(new File("src/test/resources/array/ArrayAccess1.pilar"))) produceJavaClass
//    """package com.fgwei;
//      |
//      |
//      |public class RecordDecl {
//      |  public static int main() {
//      |    int_v2:= 2130903040I ;
//      |
//      |  }
//      |}
//    """.stripMargin.trim

  class TestFile(s: FgSourceFile) {

    def produceJavaClass(expectedClassStr: String)() {
      it should ("translate >>>" + s.code + "<<< to >>>" + expectedClassStr + "<<<") in {
        val reporter = new DefaultReporter
        val translator = new Jawa2Java(reporter)
        val javaClass = translator.translate(Right(s)).values.mkString("")
        println ("-----Translated Java Class:-----\n" + javaClass + "======")
        println ("\n\n*****expected*****: \n\n" + expectedClassStr + "======")

        require(!reporter.hasErrors, reporter.problems)
        //        require(javaClass == expectedClassStr)

        /* Using String diff -> sorting in arrays causing test failures. */
//        val requireCheck: Boolean = javaClass.diff(expectedClassStr) == "" && expectedClassStr.diff(javaClass) == ""
//        require(requireCheck)
        require(compare(javaClass, expectedClassStr))
      }
    }

    /** Compares two Strings.
      * Ignores white spaces and empty lines.
      *
      * @param translated translated java code
      * @param expected expected java code
      * @return
      */
    def compare(translated: String, expected: String): Boolean = {
      val translatedLines: List[String] = (translated.split("\n") map {
        l =>
          l.trim()
      } filterNot(_ == "")).toList

      val expectedLines = (expected.split("\n") map {
        l =>
          l.trim()
      } filterNot(_ == "")).toList

      println ( "Differences1: \n||~" + (translatedLines.mkString("") diff expectedLines.mkString("")) + "~||" )
      println ( "Differences2: \n||~" + (expectedLines.mkString("") diff translatedLines.mkString("")) + "~||" )

      //      translatedLines.mkString("") == expectedLines.mkString("")
      println ("translated: " + translatedLines.toList)
      println ("expected  : " + expectedLines.toList)
      translatedLines == expectedLines
    }
  }
}
