package org.tmt.csw.cs.api

import java.util.Date

/**
 * Holds information about a specific version of a config file
 */
case class ConfigFileHistory(id: String, comment: String, time: Date)
