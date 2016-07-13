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
      |    int[][] int_arr2;
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
      |    int_arr2[i2][1] = test.s1;
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

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/array/ArrayCopy.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.lang.String;
      |import java.lang.System;
      |
      |public class ArrayCopy {
      |  public static String main() {
      |     String[] String_arr1_v0;
      |     String[] String_arr1_v1;
      |     String String_v4;
      |     int int_v5;
      |     int int_v6;
      |
      |    int_v6 = 1;
      |    int_v5 = 0;
      |    String_v4 = "phone";
      |    String_arr1_v0 = new String[int_v6];
      |    String_arr1_v0[int_v5] = String_v4;
      |    String_arr1_v1 = new String[int_v6];
      |    System.arraycopy(String_arr1_v0, int_v5, String_arr1_v1, int_v5, int_v6);
      |    String_v4 = String_arr1_v1[int_v5];
      |    return String_v4;
      |  }
      |}
    """.stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/simple/RecodDecl4_call_with_assignment.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.lang.Math;
      |
      |public class DoubleLong1 {
      |  public static float main() {
      |     double double_temp;
      |     int int_v0;
      |     float float_v0;
      |     double double_v0;
      |     double double_v2;
      |     float float_v3;
      |     float float_v5;
      |
      |    int_v0 = 1056964608;
      |    float_v5 = 1111111111.00F;
      |    double_v0 = 0.12D;
      |    double_v2 = 0.471239D;
      |    double_temp = Math.sin(double_v0);
      |    double_v0 = (double) float_v5;
      |    float_v0 = 0.1F;
      |    float_v3 = float_v5 - int_v0;
      |    return float_v0;
      |  }
      |}
    """.stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/doublelong/DoubleLong1.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.lang.Math;
      |
      |public class DoubleLong1 {
      |  public static float main() {
      |     double double_temp;
      |     int int_v0;
      |     float float_v0;
      |     double double_v0;
      |     double double_v2;
      |     float float_v3;
      |     float float_v5;
      |
      |    int_v0 = 1056964608;
      |    float_v5 = 1111111111.00F;
      |    float_v3 = float_v5 - int_v0;
      |    double_v0 = (double) float_v5;
      |    double_v2 = 0.471239D;
      |    double_v0 = double_v0 * double_v2;
      |    float_v5 = (float) double_v0;
      |    double_v0 = (double) float_v5;
      |    double_temp = Math.sin(double_v0);
      |    double_v0 = double_temp;
      |    float_v0 = (float) double_v0;
      |    return float_v0;
      |  }
      |}
    """.stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/simple/RecordDecl5_constructor.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.lang.Object;
      |import java.lang.String;
      |
      |public class FieldAccess1 {
      |  private int i1;
      |  private  FieldAccess1(String str1) {
      |     String String_v0;
      |     int int_v1;
      |
      |    super();
      |    String_v0 = str1;
      |    int_v1 = 0;
      |  }
      |
      |  private  FieldAccess1(String str1, String str2) {
      |     String String_v0;
      |     int int_v1;
      |
      |    String_v0 = str1;
      |    int_v1 = 0;
      |    super(String_v0);
      |  }
      |
      |  private int getTaint() {
      |     int int_v3;
      |
      |    int_v3 = this.i1;
      |    return int_v3;
      |  }
      |
      |  private void setTaint(int int_v3) {
      |
      |    this.i1 = int_v3;
      |  }
      |
      |  public static int main() {
      |     int int_temp;
      |     int int_v0;
      |     FieldAccess1 FieldAccess1_v1;
      |     FieldAccess1 FieldAccess1_v2;
      |     int int_v2;
      |     String s1;
      |
      |    s1 = "ConstructorString";
      |    FieldAccess1_v1 = new FieldAccess1(s1);
      |    FieldAccess1_v2 = new FieldAccess1(s1, s1);
      |    int_v0 = 1;
      |    FieldAccess1_v1.setTaint(int_v0);
      |    int_temp = FieldAccess1_v1.getTaint();
      |    int_v2 = int_temp;
      |    return int_v2;
      |  }
      |}""".stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/field/FieldAccess1.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.lang.Object;
      |
      |public class FieldAccess1 {
      |  private int i1;
      |    FieldAccess1() {
      |
      |  }
      |
      |  private int getTaint() {
      |     int int_v3;
      |
      |    int_v3 = this.i1;
      |    return int_v3;
      |  }
      |
      |  private void setTaint(int int_v3) {
      |
      |    this.i1 = int_v3;
      |  }
      |
      |  public static int main() {
      |     int int_temp;
      |     int int_v0;
      |     FieldAccess1 FieldAccess1_v1;
      |     int int_v2;
      |
      |    FieldAccess1_v1 = new FieldAccess1();
      |    int_v0 = 1;
      |    FieldAccess1_v1.setTaint(int_v0);
      |    int_temp = FieldAccess1_v1.getTaint();
      |    int_v2 = int_temp;
      |    return int_v2;
      |  }
      |}
      |""".stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/field/FieldAccess2.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.lang.Object;
      |import java.lang.String;
      |
      |public class FieldAccess2 {
      |  private String i1;
      |    FieldAccess2() {
      |
      |  }
      |
      |  private String getTaint() {
      |     String String_v3;
      |
      |    String_v3 = this.i1;
      |    return String_v3;
      |  }
      |
      |  private void setTaint(String String_v3) {
      |
      |    this.i1 = String_v3;
      |  }
      |
      |  public static String main() {
      |     String String_temp;
      |     String String_v0;
      |     FieldAccess2 FieldAccess2_v1;
      |     String String_v2;
      |
      |    FieldAccess2_v1 = new FieldAccess2();
      |    String_v0 = "test";
      |    FieldAccess2_v1.setTaint(String_v0);
      |    String_temp = FieldAccess2_v1.getTaint();
      |    String_v2 = String_temp;
      |    return String_v2;
      |  }
      |}
      |""".stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/instance/Instanceof1.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.lang.String;
      |
      |public class Instanceof1 {
      |  public static boolean main() {
      |     String String_v0;
      |     boolean boolean_v1;
      |
      |    String_v0 = "abcd";
      |    boolean_v1 = String_v0 instanceof String;
      |    return boolean_v1;
      |  }
      |}
      |""".stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/instance/Instanceof2.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.lang.Class;
      |import java.lang.Object;
      |
      |public class Instanceof2 {
      |  public static boolean main() {
      |     Object Object_v0;
      |     boolean boolean_v1;
      |
      |    Object_v0 = "abcd";
      |    boolean_v1 = Object_v0 instanceof Class;
      |    return boolean_v1;
      |  }
      |}
      |""".stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/cmp/Cmp1.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |
      |public class Cmp1 {
      |  public static boolean main() {
      |     float float_v1;
      |     float float_v2;
      |     boolean boolean_v3;
      |
      |    float_v1 = 2F;
      |    float_v2 = 3F;
      |    boolean_v3 = float_v1 < float_v2;
      |    return boolean_v3;
      |  }
      |}
      |""".stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/jump/IfJump1.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |
      |public class IfJump1 {
      |  public static int main() {
      |     int int_v1;
      |     int int_v2;
      |
      |    int_v1 = 2130903040;
      |    int_v2 = 3;
      |    if(int_v2 >= 0) {
      |    }
      |    else {
      |      int_v1 = 0;
      |    }
      |    return int_v1;
      |  }
      |}
      |""".stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/jump/IfJump2.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |
      |public class IfJump2 {
      |  public static int main() {
      |     int int_v1;
      |     int int_v2;
      |
      |    int_v1 = 2130903040;
      |    int_v2 = 3;
      |    if(int_v2 <= 0) {
      |    }
      |    else {
      |      int_v1 = 0;
      |    }
      |    return int_v1;
      |  }
      |}
      |""".stripMargin.trim



  /*new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/jump/IfJump3.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |
      |public class IfJump2 {
      |  public static int main() {
      |    int a = 4;
      |        int b;
      |        if(a < 5) {
      |            b = -1;
      |        } else if(a == 5){
      |            b = 0;
      |        } else {
      |            b = 1;
      |        }
      |
      |        String str1 = "Hello";
      |        String str2 = "World";
      |        String str3 = str1 + str2;
      |
      |        return b;
      |  }
      |}
      |""".stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/jump/IfJump5.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |
      |public class IfJump2 {
      |  public static int main() {
      |    int a = 4;
      |        int b;
      |        if(a < 5) {
      |            b = -1;
      |        } else if(a == 5){
      |            b = 0;
      |        } else {
      |            b = 1;
      |        }
      |
      |        String str1 = "Hello";
      |        String str2 = "World";
      |        String str3 = str1 + str2;
      |
      |        if (str3.equals("test")) {
      |            str1 = "Hello Again";
      |        } else if (str3.equals("test1")) {
      |            str1 = "Hello Again1";
      |        } else {
      |            str1 = str2;
      |        }
      |
      |        return b;
      |  }
      |}
      |""".stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/jump/IfJump_with_for_loop.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |
      |public class IfJump2 {
      |  public static int main() {
      |    int a = 4;
      |        int b;
      |        if(a < 5) {
      |            b = -1;
      |        } else if(a == 5){
      |            b = 0;
      |        } else {
      |            b = 1;
      |        }
      |
      |        String str1 = "Hello";
      |        String str2 = "World";
      |        String str3 = str1 + str2;
      |
      |        if (str3.equals("test")) {
      |            str1 = "Hello Again";
      |        } else if (str3.equals("test1")) {
      |            str1 = "Hello Again1";
      |        } else {
      |            str1 = str2;
      |        }
      |
      |        int sum = 0;
      |        for(int i = 0; i < 100; i++) {
      |            sum = sum + i;
      |        }
      |
      |        return b;
      |  }
      |}
      |""".stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/jump/IfJump_nested.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |
      |public class IfJump2 {
      |  public static int main() {
      |    String str1 = "Hello";
      |
      |        int sum = 50;
      |
      |        if(sum < 100 ) {
      |            int c = sum + 100;
      |            if (c < 200) {
      |                str1 = "less than 200";
      |            } else {
      |                str1 = "greater than 200";
      |            }
      |        }
      |
      |        return sum;
      |  }
      |}
      |""".stripMargin.trim*/

  /*new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/simple/IfJump_nested_2.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |
      |public class IfJump2 {
      |  public static int main() {
      |     String str1 = "Hello";
      |
      |        int sum = 50;
      |
      |        if(sum < 100 ) {
      |            int c = sum + 100;
      |            if (c < 200) {
      |                str1 = "less than 200";
      |            } else {
      |                str1 = "greater than 200";
      |            }
      |        }
      |
      |        sum = sum * 2;
      |        return sum;
      |  }
      |}
      |""".stripMargin.trim*/

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/simple/IfJump_nested_3_with_else.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.lang.String;
      |
      |public class IfJump1 {
      |  public static int main() {
      |     int int_v0;
      |     int int_v3;
      |     int int_v4;
      |     String String_v2;
      |     int int_v1;
      |
      |    String_v2 = "Hello";
      |    int_v3 = 50;
      |    int_v4 = 100;
      |    if(int_v3 >= int_v4) {
      |      int_v1 = int_v3 + 500;
      |      int_v4 = 600;
      |      if(int_v1 >= int_v4) {
      |        String_v2 = "greater than 600";
      |      }
      |      else {
      |        String_v2 = "less than 600";
      |      }
      |    }
      |    else {
      |      int_v0 = int_v3 + 100;
      |      int_v4 = 200;
      |      if(int_v0 >= int_v4) {
      |        String_v2 = "greater than 200";
      |      }
      |      else {
      |        String_v2 = "less than 200";
      |      }
      |    }
      |    int_v3 = int_v3 * 2;
      |    return int_v3;
      |  }
      |}
      |""".stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/simple/IfJump_nested_3_with_elseIf.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.lang.String;
      |
      |public class IfJump1 {
      |  public static int main() {
      |     int int_v5;
      |     int int_v0;
      |     int int_v3;
      |     int int_v4;
      |     String String_v2;
      |     int int_v1;
      |
      |    int_v5 = 200;
      |    int_v4 = 100;
      |    String_v2 = "Hello";
      |    int_v3 = 50;
      |    if(int_v3 >= int_v4) {
      |      if(int_v3 <= int_v4) {
      |        int_v1 = int_v3 + 500;
      |        int_v4 = 600;
      |        if(int_v1 >= int_v4) {
      |          String_v2 = "greater than 600";
      |        }
      |        else {
      |          String_v2 = "less than 600";
      |        }
      |        int_v3 = int_v3 + 9;
      |      }
      |      else {
      |        if(int_v3 >= int_v5) {
      |          int_v1 = int_v3 + 500;
      |          int_v4 = 600;
      |          if(int_v1 >= int_v4) {
      |            String_v2 = "greater than 600";
      |          }
      |          else {
      |            String_v2 = "less than 600";
      |          }
      |          int_v3 = int_v3 + 9;
      |        }
      |        else {
      |          String_v2 = "equal to 50";
      |        }
      |      }
      |    }
      |    else {
      |      int_v0 = int_v3 + 100;
      |      if(int_v0 >= int_v5) {
      |        String_v2 = "greater than 200";
      |      }
      |      else {
      |        String_v2 = "less than 200";
      |      }
      |    }
      |    int_v3 = int_v3 * 2;
      |    return int_v3;
      |  }
      |}
      |""".stripMargin.trim

  new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/simple/IfJump5.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.lang.String;
      |import java.lang.StringBuilder;
      |
      |public class IfJump5 {
      |  public static int main() {
      |     String String_v3;
      |     int int_v5;
      |     boolean boolean_temp;
      |     String String_v4;
      |     int int_v0;
      |     boolean boolean_v5;
      |     String String_v5;
      |     StringBuilder StringBuilder_temp;
      |     StringBuilder StringBuilder_v5;
      |     String String_v2;
      |     int int_v1;
      |     String String_temp;
      |
      |    int_v5 = 5;
      |    int_v0 = 4;
      |    if(int_v0 >= int_v5) {
      |      if(int_v0 != int_v5) {
      |        int_v1 = 1;
      |      }
      |      else {
      |        int_v1 = 0;
      |      }
      |    }
      |    else {
      |      int_v1 = 15;
      |    }
      |    String_v2 = "Hello";
      |    String_v3 = "World";
      |    StringBuilder_v5 = new StringBuilder();
      |    StringBuilder_temp = StringBuilder_v5.append(String_v2);
      |    StringBuilder_v5 = StringBuilder_temp;
      |    StringBuilder_temp = StringBuilder_v5.append(String_v3);
      |    StringBuilder_v5 = StringBuilder_temp;
      |    String_temp = StringBuilder_v5.toString();
      |    String_v4 = String_temp;
      |    String_v5 = "test";
      |    boolean_temp = String_v4.equals(String_v5);
      |    boolean_v5 = boolean_temp;
      |    if(boolean_v5 == false) {
      |      String_v5 = "test1";
      |      boolean_temp = String_v4.equals(String_v5);
      |      boolean_v5 = boolean_temp;
      |      if(boolean_v5 == false) {
      |        String_v2 = String_v3;
      |      }
      |      else {
      |        String_v2 = "Hello Again1";
      |      }
      |    }
      |    else {
      |      String_v2 = "Hello Again";
      |    }
      |    return int_v1;
      |  }
      |}
      |""".stripMargin.trim


/*
   private int testPilar() {
        String str1 = "Hello";

        int sum = 50;

        if(sum < 100 ) {
            str1 = "less than 100";

        } else if (sum < 200){
            str1 = "less than 200";
        } else {
            str1 = "greater than 200";
        }

        sum = sum * 2;
        return sum;
    }*/
  new FgSourceFile(new PlainFile(new File("src/test/resources/simple/ifelseifelse.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |import java.lang.String;
      |
      |public class IfJump1 {
      |  public static int main() {
      |     String String_v0;
      |     int int_v2;
      |     int int_v1;
      |
      |    String_v0 = "Hello";
      |    int_v1 = 50;
      |    int_v2 = 100;
      |    if(int_v1 >= int_v2) {
      |      int_v2 = 200;
      |      if(int_v1 >= int_v2) {
      |        String_v0 = "greater than 200";
      |      }
      |      else {
      |        String_v0 = "less than 200";
      |      }
      |    }
      |    else {
      |      String_v0 = "less than 100";
      |    }
      |    int_v1 = int_v1 * 2;
      |    return int_v1;
      |  }
      |}""".stripMargin.trim

  /*
 public class HelloWorld{

     public static void main(String []args){

        System.out.println("getSum: " + getSum());
        System.out.println("getSumWhile: " + getSumWhile());
        System.out.println("getSumX2: " + getSumX2());
        System.out.println("getSumConditional: " + getSumConditional());
        System.out.println("getSumConditional1: " + getSumConditional1());
        System.out.println("nestedWhile: " + nestedWhile());
        System.out.println("forConditional: " + forConditional());
        System.out.println("doWhileSum: " + doWhileSum());

     }

    private static  int getSum() {
        int sum = 0;
        for (int i = 0; i < 50 ; i++) {
            sum = sum + i;
        }
        return sum;
    }
    private  static int getSumWhile() {
        int sum = 0;
        int i = 0;
        while(i < 50){
            sum = sum + i;
            i = i + 1;
        }
        return sum;
    }

    private static  int getSumX2() {
        int sum = 0;
        int i = 0;
        while(i < 50){
            sum = sum + i;
            i = i + 1;
        }
        sum = sum * 2;
        return sum;
    }

    private  static int getSumConditional() {
        int sum = 0;
        int i = 0;
        while(i < 50){
            sum = sum + i;
            i = i + 1;
            if(sum > 20) {
                sum = sum * 2;
            }
        }
        sum = sum * 2;
        return sum;
    }

    private  static int getSumConditional1() {
        int sum = 0;
        int i = 0;
        while(i < 50){
            sum = sum + i;
            i = i + 1;
            if(sum > 20) {
                sum = sum * 2;
            }else {
                sum = sum / 2;
            }
        }
        sum = sum * 2;
        return sum;
    }

    private static  int nestedWhile() {
        int sum = 0;
        int i = 0;
        while(i < 50){
            sum = sum + i;
            int j = 0;
            while ( j < i) {
                sum = sum + j;
                j++;
            }

            i = i + 1;

        }
        sum = sum * 2;
        return sum;
    }

    private  static int forConditional() {
        int sum = 0;
        for (int i = 0; i < 50 ; i++) {
            sum = sum + i;
            if(sum > 20) {
                sum = sum * 2;
            } else {
                sum = sum / 2;
            }
        }
        sum = sum * 2;
        return sum;
    }

    private static  int doWhileSum() {
        int sum = 0;
        int i = 0;
        do {
            sum = sum + i;
            i ++;
        } while(i < 50);

        return sum;
    }

    private static int nestedWhileConditional() {
        int sum = 0;
        int i = 0;
        while(i < 50){
            sum = sum + i;
            int j = 0;
            while ( j < i) {
                sum = sum + j;
                if(sum >1000){
                    sum = sum - 5;
                }else {
                    sum = sum + 1;
                }
                j++;
            }
            i = i + 1;
        }
        sum = sum * 2;
        return sum;
    }

    private static int nestedWhile2() {
        int sum = 0;
        int i = 0;
        while(i < 50){
            sum = sum + i;
            i = i + 1;
            int j = 0;
            while ( j < i) {
                sum = sum + j;
                j++;
            }
        }
        return sum;
    }
}
*/
  new FgSourceFile(new PlainFile(new File("src/test/resources/loops/forLoop1.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |
      |public class ForLoop1 {
      |  public static int main() {
      |     int int_v2;
      |     int int_v1;
      |     int int_v0;
      |
      |    int_v1 = 0;
      |    int_v0 = 0;
      |    int_v2 = 50;
      |    while(!(int_v0 >= int_v2)) {
      |      int_v1 = int_v1 + int_v0;
      |      int_v0 = int_v0 + 1;
      |      int_v2 = 50;
      |    }
      |    return int_v1;
      |  }
      |
      |  private int whileLoop() {
      |     int int_v2;
      |     int int_v1;
      |     int int_v0;
      |
      |    int_v1 = 0;
      |    int_v0 = 0;
      |    int_v2 = 50;
      |    while(!(int_v0 >= int_v2)) {
      |      int_v1 = int_v1 + int_v0;
      |      int_v0 = int_v0 + 1;
      |      int_v2 = 50;
      |    }
      |    return int_v1;
      |  }
      |
      |  private int whileLoop2() {
      |     int int_v2;
      |     int int_v1;
      |     int int_v0;
      |
      |    int_v1 = 0;
      |    int_v0 = 0;
      |    int_v2 = 50;
      |    while(!(int_v0 >= int_v2)) {
      |      int_v1 = int_v1 + int_v0;
      |      int_v0 = int_v0 + 1;
      |      int_v2 = 50;
      |    }
      |    int_v1 = int_v1 * 2;
      |    return int_v1;
      |  }
      |
      |  private int whileLoopConditional() {
      |     int int_v2;
      |     int int_v1;
      |     int int_v0;
      |
      |    int_v1 = 0;
      |    int_v0 = 0;
      |    int_v2 = 50;
      |    while(!(int_v0 >= int_v2)) {
      |      int_v1 = int_v1 + int_v0;
      |      int_v0 = int_v0 + 1;
      |      int_v2 = 20;
      |      if(int_v1 <= int_v2) {
      |      }
      |      else {
      |        int_v1 = int_v1 * 2;
      |      }
      |      int_v2 = 50;
      |    }
      |    int_v1 = int_v1 * 2;
      |    return int_v1;
      |  }
      |
      |  private int nestedWhile2() {
      |     int int_v2;
      |     int int_v0;
      |     int int_v3;
      |     int int_v1;
      |
      |    int_v2 = 0;
      |    int_v0 = 0;
      |    int_v3 = 50;
      |    while(!(int_v0 >= int_v3)) {
      |      int_v2 = int_v2 + int_v0;
      |      int_v0 = int_v0 + 1;
      |      int_v1 = 0;
      |      while(!(int_v1 >= int_v0)) {
      |        int_v2 = int_v2 + int_v1;
      |        int_v1 = int_v1 + 1;
      |      }
      |      int_v3 = 50;
      |    }
      |    return int_v2;
      |  }
      |
      |  private int whileLoopConditional1() {
      |     int int_v2;
      |     int int_v1;
      |     int int_v0;
      |
      |    int_v1 = 0;
      |    int_v0 = 0;
      |    int_v2 = 50;
      |    while(!(int_v0 >= int_v2)) {
      |      int_v1 = int_v1 + int_v0;
      |      int_v0 = int_v0 + 1;
      |      int_v2 = 20;
      |      if(int_v1 <= int_v2) {
      |        int_v1 = int_v1 / 2;
      |      }
      |      else {
      |        int_v1 = int_v1 * 2;
      |      }
      |      int_v2 = 50;
      |    }
      |    int_v1 = int_v1 * 2;
      |    return int_v1;
      |  }
      |
      |  private int nestedWhile() {
      |     int int_v2;
      |     int int_v0;
      |     int int_v3;
      |     int int_v1;
      |
      |    int_v2 = 0;
      |    int_v0 = 0;
      |    int_v3 = 50;
      |    while(!(int_v0 >= int_v3)) {
      |      int_v2 = int_v2 + int_v0;
      |      int_v1 = 0;
      |      while(!(int_v1 >= int_v0)) {
      |        int_v2 = int_v2 + int_v1;
      |        int_v1 = int_v1 + 1;
      |      }
      |      int_v0 = int_v0 + 1;
      |      int_v3 = 50;
      |    }
      |    int_v2 = int_v2 * 2;
      |    return int_v2;
      |  }
      |
      |  private int nestedWhileConditional() {
      |     int int_v2;
      |     int int_v0;
      |     int int_v3;
      |     int int_v1;
      |
      |    int_v2 = 0;
      |    int_v0 = 0;
      |    int_v3 = 50;
      |    while(!(int_v0 >= int_v3)) {
      |      int_v2 = int_v2 + int_v0;
      |      int_v1 = 0;
      |      while(!(int_v1 >= int_v0)) {
      |        int_v2 = int_v2 + int_v1;
      |        int_v3 = 1000;
      |        if(int_v2 <= int_v3) {
      |          int_v2 = int_v2 + 1;
      |        }
      |        else {
      |          int_v2 = int_v2 + 251;
      |        }
      |        int_v1 = int_v1 + 1;
      |      }
      |      int_v0 = int_v0 + 1;
      |      int_v3 = 50;
      |    }
      |    int_v2 = int_v2 * 2;
      |    return int_v2;
      |  }
      |}""".stripMargin.trim

  /*new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/jump/IfJump_nested_3_with_else_2.pilar"))) produceJavaClass
    """package com.fgwei;
      |
      |
      |public class IfJump2 {
      |  public static int main() {
      |       String str1 = "Hello";
      |
      |        int sum = 50;
      |
      |        if(sum < 100 ) {
      |            int c = sum + 100;
      |            if (c < 200) {
      |                str1 = "less than 200";
      |            } else {
      |                str1 = "greater than 200";
      |            }
      |        } else {
      |            int d = sum + 500;
      |            if (d < 600) {
      |                str1 = "less than 600";
      |            } else {
      |                str1 = "greater than 600";
      |            }
      |            sum = sum + 9;
      |        }
      |
      |        sum = sum * 2;
      |        return sum;
      |  }
      |}
      |""".stripMargin.trim*/

  /* new FgSourceFile(new PlainFile(new File("/Users/atuladhar/projects/jawa2java/src/test/resources/simple/IfJump_with_for_loop.pilar"))) produceJavaClass
     """package com.fgwei;
       |
       |
       |public class IfJump2 {
       |  public static int main() {
       |    int a = 4;
       |        int b;
       |        if(a < 5) {
       |            b = -1;
       |        } else if(a == 5){
       |            b = 0;
       |        } else {
       |            b = 1;
       |        }
       |
       |        String str1 = "Hello";
       |        String str2 = "World";
       |        String str3 = str1 + str2;
       |
       |        if (str3.equals("test")) {
       |            str1 = "Hello Again";
       |        } else if (str3.equals("test1")) {
       |            str1 = "Hello Again1";
       |        } else {
       |            str1 = str2;
       |        }
       |
       |        int sum = 0;
       |        for(int i = 0; i < 100; i++) {
       |            sum = sum + i;
       |        }
       |
       |        return b;
       |  }
       |}
       |""".stripMargin.trim*/

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
        println("Source File is: " + s.file.canonicalPath)
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
