/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package synthesis
package rules

import solvers._
import purescala.Expressions._
import purescala.Definitions._
import purescala.ExprOps._
import purescala.Constructors._
import purescala.Common._
import scala.concurrent.duration._

import leon.frontends.scalac.{ScalaCompilerReference, ScalaCompiler}

case object UserInput extends Rule("Manual input") {
  val compiler = ScalaCompilerReference.compiler


  def instantiateOn(implicit hctx: SearchContext, p: Problem): Traversable[RuleInstantiation] = {
    if (p.as.nonEmpty && p.xs.nonEmpty) {
      List(new RuleInstantiation(this.name) {
        val sctx = hctx.sctx
        val ctx  = sctx.context

        private val outerSolution = {
          val part = new PartialSolution(hctx.search.g, true)
          e : Expr => part.solutionAround(hctx.currentNode)(e).getOrElse {
            sctx.reporter.fatalError("Unable to create outer solution")
          }
        }

        def readExpression(): Option[Expr] = {
          import compiler._
          val runner = new Run()

          println("Tell me what to do, oh Great Oracle")
          val code = scala.io.StdIn.readLine()
          if (code == null) return None // Ctrl-D

          // THIS GENERATES TREES
          // val unitparser = newUnitParser(code)
          // println(unitparser.simpleExpr)
          // println(compiler.leonExtraction.compiledUnits.last)


          // THIS PARSES JUST FINE, but you need a "context" around
          val as = p.as map { a => a + ": " + a.getType } mkString(", ")
          // TODO: just take one, I'm not really sure when you can have a
          // list...
          val ret = p.xs(0).getType.toString
          val codeinobject = s"import AssociativeList._; object A { def entered($as): $ret = $code }"
          val compilationUnit = newCompilationUnit(codeinobject, "<console>")

          try {
            runner.compileUnits(List(compilationUnit), runner.parserPhase)
          } catch {
            case e: Exception => e.printStackTrace()
          }

          compiler.leonExtraction.setImports(compiler.saveImports.imports)
          val compilationOutput = compiler.leonExtraction.compiledUnits.last
          val definition = compilationOutput.modules.last.definedFunctions.last


          // Apparently the resulting expression has fresh identifiers. We can
          // easily fix it for the input variables:
          val varmap: Map[String, Identifier] = (p.as.map{ a => a.toString -> a }).toMap
          val body = preMap({ expr => expr match {
            case Variable(id) if varmap contains id.name =>
              Some(Variable(varmap(id.name)))
            case _ =>
              None
          } }) (definition.fullBody)

          //assert(!(p.as forall { variablesOf(definition.fullBody).contains(_) }))
          //assert(variablesOf(body) forall { p.as.contains(_) })

          if (!reporter.hasErrors) {
            Some(body)
          } else {
            None
          }
        }

        def apply(hctx: SearchContext): RuleApplication = {
          readExpression() map { e : Expr =>
            RuleClosed(Solution(BooleanLiteral(true), Set(), e))//, isTrusted = false)
          } getOrElse(RuleFailed())
        }
      })
    } else {
      None
    }
  }
}

