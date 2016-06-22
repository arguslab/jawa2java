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

    val sortedImports = imports.toList.sortWith((x, y) => x.baseTyp < y.baseTyp)

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
    fieldTemplate.add("attrTyp", fd.typ.typ.name)
    fieldTemplate.add("attrName", fd.fieldName)
    addImport(fd.typ.typ, imports)
    fieldTemplate
  }

  def visitMethodDeclaration(md: MethodDeclaration, imports: MSet[JawaType]): ST = {
    val methodTemplate = template.getInstanceOf("MethodDecl")
    val bodyStatements: MList[(Int, ST)] = mlistEmpty

    methodTemplate.add("accessFlag", AccessFlag.toString(AccessFlag.getAccessFlags(md.accessModifier)))
    methodTemplate.add("retTyp", md.returnType.typ.name)
    methodTemplate.add("methodName", md.name)

    md.body match {
      case resolvedBody: ResolvedBody =>
        val localVars: Array[ST] = resolvedBody.locals.map {
          lv =>
            visitLocalVarDeclaration(lv, imports)
        }.toArray

        methodTemplate.add("localVars", localVars )

        resolvedBody.locations foreach {
          loc =>
            println ("Location Symbol is: " + loc.locationSymbol)
            println ("Location Statement is: " + loc.statement)

            loc.statement match {
              case assign: AssignmentStatement =>
                bodyStatements += ((loc.locationIndex, assignmentStatement(assign)))

              case ret: ReturnStatement =>
                ret.varOpt match {
                  case Some(v) =>
                    val returnTemplate = template.getInstanceOf("ReturnStatement")
                    println ("Return Statement is: " + v.varName)
                    returnTemplate.add("varName", v.varName)

                    bodyStatements += ((loc.locationIndex, returnTemplate))
                  case None =>
                }

              case _ =>
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

    val paramTemplates: Array[ST] = md.paramlist.map{
      param =>
        val paramTemplate = visitParamDeclaration(param, imports)
        paramTemplate
    }.toArray

    methodTemplate.add("params", paramTemplates)

    methodTemplate
  }

  private def assignmentStatement(as: AssignmentStatement): ST = {
    val assignmentTemplate = template.getInstanceOf("AssignmentStatement")
    val lhs: Option[ST] = expression_lhs(as.lhs)
    val rhs: Option[ST] = expression_rhs(as.rhs)

    lhs match {
      case Some(l) =>
        assignmentTemplate.add("lhs", l)
      case None =>
    }

    rhs match {
      case Some(r) =>
        assignmentTemplate.add("rhs", r)
      case None =>
    }

    assignmentTemplate
  }

  private def expression_lhs(lhs: Expression with LHS): Option[ST] = {
    lhs match {
      case ne: NameExpression =>
        Some(nameExpression(ne))
      case _ => None
    }
  }

  private def expression_rhs(rhs: Expression with RHS): Option[ST] = {
    rhs match {
      case ne: NameExpression =>
        Some(nameExpression(ne))

      case newExp: NewExpression =>
        println ("in new rhs expressions" + newExp.typ.simpleName)
        val newTemplate: ST = template.getInstanceOf("NewExpression")
        newTemplate.add("baseType", newExp.typ.simpleName)
        Some(newTemplate)

      case le: LiteralExpression =>
        literalExpression(le)

      case _ =>
        println ("Case non in rhs." + rhs.getClass)
        None
    }
  }

  private def nameExpression(ne: NameExpression): ST = {
    val nameTemplate = template.getInstanceOf("NameExpression")
    nameTemplate.add("name", ne.name)
    nameTemplate
  }

  private def literalExpression(le: LiteralExpression): Option[ST] = {
    le.lastToken.tokenType match  {
      case STRING_LITERAL =>
        val literalTemplate = template.getInstanceOf("StringLiteral")
        literalTemplate.add("str", le.getString)
        Some(literalTemplate)
      case _ =>
        None
    }
  }

  def visitLocalVarDeclaration(lvd: LocalVarDeclaration, imports: MSet[JawaType] ): ST = {
    val fieldTemplate = template.getInstanceOf("FieldDecl")

    fieldTemplate.add("accessFlag", AccessFlag.toString(AccessFlag.getAccessFlags(lvd.accessModifier)))
    lvd.typOpt match {
      case Some(typ) =>
        fieldTemplate.add("attrTyp", typ.typ.simpleName)
        fieldTemplate.add("attrName", lvd.varSymbol.varName)
      case None =>
    }

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
