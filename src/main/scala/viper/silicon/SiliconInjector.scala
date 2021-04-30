package viper.silicon

import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.{ScClassImpl, SyntheticMembersInjector}
import com.intellij.openapi.diagnostic.Logger
import org.jetbrains.plugins.scala.lang.psi.types.TypePresentationContext

final class SiliconInjector extends SyntheticMembersInjector {
  private val qualifiedName = "flyweight"

  private val logger = Logger.getInstance(classOf[SiliconInjector])
  logger.info("SiliconInjector plugin was started.")

  override def needsCompanionObject(source: ScTypeDefinition): Boolean =
    source.findAnnotationNoAliases(qualifiedName) != null

  override def injectFunctions(source: ScTypeDefinition): Seq[String] = {
    source match {
      case obj: ScObject =>
        obj.fakeCompanionClassOrCompanionClass match {
          case clazz: ScClass if clazz.findAnnotationNoAliases(qualifiedName) != null => {
            val params = clazz.constructor.get.parameterList.params

            val applyArgs =
              params
                .map(p =>s"${p.name}: ${p.`type`().get.presentableText(TypePresentationContext.emptyContext)}")
                .mkString(", ")

            val applyDef = s"def apply($applyArgs): ${clazz.getName} = ???"

            val unapplyTypes =
              params
                .map(_.`type`().get.presentableText(TypePresentationContext.emptyContext))
                .mkString(", ")

            val unapplyDef =
              if (params.isEmpty) {
                s"def unapply(obj: ${clazz.getName}): Boolean = ???"
              } else if (params.lengthCompare(1) == 0) {
                s"def unapply(obj: ${clazz.getName}): Option[$unapplyTypes] = ???"
              } else {
                s"def unapply(obj: ${clazz.getName}): Option[($unapplyTypes)] = ???"
              }

            logger.info(s"Generating apply for ${clazz.getName}: $applyDef")
            logger.info(s"Generating unapply for ${clazz.getName}: $unapplyDef")

            Seq(applyDef, unapplyDef)
          }
          case _ => Seq.empty
        }

      case _ => Seq.empty
    }
  }

  override def injectMembers(source: ScTypeDefinition): Seq[String] = {
    source match {
      case clazz: ScClass if clazz.findAnnotationNoAliases(qualifiedName) != null =>
        val params = clazz.constructor.get.parameterList.params

        val copyArgs =
          params
            .map(p =>s"${p.name}: ${p.`type`().get.presentableText(TypePresentationContext.emptyContext)} = ${p.name}")
            .mkString(", ")

        val copyDef = s"def copy($copyArgs): ${clazz.getName} = ???"

        logger.info(s"Generating copy for ${clazz.getName}: $copyDef")

        Seq(copyDef)
      case _ =>
        Seq.empty
    }
  }
}