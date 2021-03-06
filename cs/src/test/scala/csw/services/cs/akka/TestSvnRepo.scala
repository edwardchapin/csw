package csw.services.cs.akka

import java.io.File

import akka.actor.{ActorRefFactory, ActorSystem}
import csw.services.cs.core.ConfigManager
import csw.services.cs.core.svn.SvnConfigManager

/**
 * Utility class to create a temporary Svn repository for use in testing.
 */
object TestSvnRepo {

  private def resetRepo(settings: ConfigServiceSettings)(implicit context: ActorRefFactory): Unit = {
    // XXX FIXME TODO: Use generated temp dirs, not settings
    //    println(s"Using test svn repo at = ${settings.mainRepository}")
    if (settings.mainRepository.getScheme != "file")
      throw new RuntimeException(s"Please specify a file URI for csw.services.cs.main-repository for testing")

    val svnMainRepo = new File(settings.mainRepository.getPath)
    // Delete the main and local test repositories (Only use this in test cases!)
    SvnConfigManager.deleteDirectoryRecursively(svnMainRepo)
    SvnConfigManager.initSvnRepo(svnMainRepo)
  }

  /**
   * Creates a temporary test Svn repository and a bare main repository for push/pull.
   * Any previous contents are deleted.
   *
   * @return a new ConfigManager set to manage the newly created Svn repositories
   */
  def getConfigManager(settings: ConfigServiceSettings = ConfigServiceSettings(ActorSystem()))(implicit context: ActorRefFactory): ConfigManager = {
    resetRepo(settings)
    SvnConfigManager(settings.mainRepository, settings.name)
  }
}
