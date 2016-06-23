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
      |import com.fgwei.RecordDecl1;
      |import com.fgwei.Test;
      |import java.io.File;
      |import java.lang.String;
      |import java.util.ArrayList;
      |
      |public class RecordDecl {
      |  private int i1;
      |   int i3;
      |   int[] int_arr1;
      |   static String str1;
      |
      |  public static int main(String param1, ArrayList arr_param_1) {
      |    int i2;
      |    String s1;
      |    String s2;
      |    File file1;
      |    float f1;
      |    String[] String_arr1;
      |    int[] int_arr2;
      |    Test test;
      |
      |    test = new Test();
      |    s1 = "testing";
      |    s2 = new String();
      |    i2 = 55;
      |    f1 = 1.5;
      |    String_arr1 = new String[i2];
      |    RecordDecl.str1 = "StringStatic";
      |    RecordDecl1.str1 = "StringStatic_import";
      |    s2 = test.s1;
      |    return i2;
      |  }
      |}
    """.stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/array/ArrayAccess1.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |
      |public class ArrayAccess1 {
      |  public static int main() {
      |     int int_v0;
      |     int int_v1;
      |     int[] int_arr1_v1;
      |     int int_v2;
      |     int int_v3;
      |
      |    int_v2 = 2130903040;
      |    int_v1 = 3;
      |    int_arr1_v1 = new int[int_v1];
      |    int_v3 = 0;
      |    int_arr1_v1[int_v3] = int_v2;
      |    int_v3 = int_arr1_v1[int_v3];
      |    return int_v3;
      |  }
      |}
    """.stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/array/ArrayAccess2.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.lang.String;
      |
      |public class ArrayAccess2 {
      |  public static String main() {
      |     int int_v1;
      |     String[] String_arr1_v1;
      |     int int_v3;
      |     String String_v3;
      |     String String_v4;
      |
      |    int_v1 = 3;
      |    String_arr1_v1 = new String[int_v1];
      |    int_v3 = 0;
      |    String_v4 = "element 1 is tainted:";
      |    String_arr1_v1[int_v3] = String_v4;
      |    String_v3 = String_arr1_v1[int_v3];
      |    return String_v3;
      |  }
      |}
    """.stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/array/ArrayAccess3.pilar"))) produceJavaClass
  """package com.fgwei;
    |
    |import java.lang.String;
    |
    |public class ArrayAccess3 {
    |  public static String main() {
    |     int int_v1;
    |     String[] String_arr1_v1;
    |     int int_v2;
    |     int int_v3;
    |     String String_v3;
    |     String String_v4;
    |     int int_v5;
    |
    |    int_v5 = 2;
    |    int_v2 = 0;
    |    int_v1 = 2130903040;
    |    int_v1 = 3;
    |    String_arr1_v1 = new String[int_v1];
    |    int_v3 = 0;
    |    String_v4 = "element 1 is tainted:";
    |    String_arr1_v1[int_v3] = String_v4;
    |    int_v3 = 1;
    |    String_v4 = "phone";
    |    String_arr1_v1[int_v3] = String_v4;
    |    String_v4 = "neutral text";
    |    String_arr1_v1[int_v5] = String_v4;
    |    String_v3 = String_arr1_v1[int_v5];
    |    return String_v3;
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
