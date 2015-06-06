package leon
package synthesis
package graph

import java.io._

import leon.utils._
import leon.purescala._
import leon.purescala.ExprOps._
import leon.purescala.Expressions._
import leon.purescala.Constructors._
import leon.purescala.PrinterHelpers._
import leon.purescala.Common._

class AdvancedManualSearch(ctx: LeonContext, ci: ChooseInfo, problem: Problem, costModel: CostModel, initCmd: Option[String]) extends ManualSearch(ctx, ci, problem, costModel, initCmd) {

  val problemFile = "./current.problem"
  val codeFile    = "./current.scala"

  def solutionOf(n: Node, current: Node): Option[Solution] = {
    // TODO: find a nice(r) way to show current hole. This does the trick but
    // gives some error:
    // Error: don't know how to handle ???(List) in Evaluator (class leon.purescala.Expressions$NoTree).

    // if (n == current) {
    //   val tpe = tupleTypeWrap(n.p.xs.map(_.getType))
    //   return Some(Solution(BooleanLiteral(true), Set(), NoTree(tpe)))
    // }

    n.solutions.flatMap ( sols =>  // If solved
      if (sols.isDefinedAt(n.selectedSolution)) {
        Some(sols(n.selectedSolution))
      } else {
        None
      }).orElse { n match { // If not solved
        case _ if (n.isDeadEnd) =>
          Some(Solution.failed(n.p))
        case an: AndNode if (n.isExpanded && n.selected != Nil) =>
          val subSols = n.descendents.collect {
            case d if n.selected contains d =>
              solutionOf(d, current).toStream
          }
          n.composeSolutions(subSols).headOption
        case on: OrNode if (n.isExpanded) =>
          val subSols = n.descendents.collect {
            case d if d.isExpanded || d.isSolved =>
              solutionOf(d, current).toStream
          }

          if (subSols.size > 0) {
            n.composeSolutions(subSols).headOption
          } else {
            Some(Solution.chooseComplete(n.p))
          }
        case _ =>
          Some(Solution.chooseComplete(n.p))
      }
    }
  }


  def solutionCode(sctx: SynthesisContext, sol: Solution): String = {
    val ChooseInfo(fd, pc, src, ch) = ci

    // TODO: it looks better after simplifying names too, but it will not match
    // the names in the problem representation
    // We have to somehow apply the same renaming at displayProblem

    // val solCode = sol.toSimplifiedExpr(sctx.context, sctx.program)
    val solCode = Simplifiers.namePreservingBestEffort(sctx.context, sctx.program)(sol.toExpr)

    val (defs, expr) = liftClosures(solCode)

    val fInt = new FileInterface(sctx.context.reporter)

    val nfd = fd.duplicate

    // TODO: same as above: it would be great to simplify names too
//    nfd.body = nfd.body.map(b => Simplifiers.bestEffort(sctx.context, sctx.program)(postMap{
    nfd.body = nfd.body.map(b => Simplifiers.namePreservingBestEffort(sctx.context, sctx.program)(postMap{
      case ch if ch == ci.ch && ch.getPos == ci.ch.getPos =>
        Some(expr)
      case _ =>
        None
    }(b)))

    val fds = nfd :: defs.toList.sortBy(_.id.name)

    (fds map {x => ScalaPrinter(x)}).mkString("\n")
  }

  def displayCode(n: Node, sctx: SynthesisContext): String = {
    (solutionOf(g.root, n) map { sol : Solution => solutionCode(sctx, sol) }).
      getOrElse("?")

  }

  def printCode(n: Node, sctx: SynthesisContext) {
    val contents = displayCode(n: Node, sctx)
    Some(new PrintWriter(codeFile)).foreach{p => p.write(contents); p.close}
  }

  def displayProblem(n: Node): String = {
    val indent = "  "

    val p = n.p

    val pcws = and(p.ws, p.pc)
    var result = ""
    result += "Synthesizing " + p.xs.mkString(", ") + "\n"

    result += "Available variables:\n"
    result += indent + p.as.mkString(", ") + "\n"

    if (pcws != BooleanLiteral(true)) {
      result += "Path conditions:\n"
      result += indent + pcws.toString.replace("&& ", "&&\n"+indent) + "\n"
    }

    result += "The solution has to meet:\n"
    var phistring = ScalaPrinter(p.phi, PrinterOptions(baseIndent = 1))
    phistring = phistring.replace("&&", "&&\n"+indent)
    result += indent + phistring + "\n"

    result
  }

  def printProblem(n: Node) {
    val contents = displayProblem(n: Node)
    Some(new PrintWriter(problemFile)).foreach{p => p.write(contents); p.close}
  }

  override def doStep(n: Node, sctx: SynthesisContext) = {
    super.doStep(n, sctx)
    printProblem(n)
    printCode(n, sctx)
  }
}
