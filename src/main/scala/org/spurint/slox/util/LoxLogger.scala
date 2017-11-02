package org.spurint.slox.util

import java.util.Locale
import org.slf4j.LoggerFactory

trait LoxLogger {
  private val logger = LoggerFactory.getLogger(
  "(?<!^)[A-Z]".r
    .replaceAllIn(getClass.getSimpleName, m => "-" + m.group(0))
    .toLowerCase(Locale.US)
    .replaceAll("\\$", "")
  )

  private def logMsg(line: Int, msg: => Any): String = s"$line:$msg"

  protected def debug(line: HasLineInfo, msg: => Any): Unit = if (logger.isDebugEnabled()) logger.debug(logMsg(line.line, msg))
  protected def info(line: HasLineInfo, msg: => Any): Unit = if (logger.isInfoEnabled()) logger.info(logMsg(line.line, msg))
  protected def warn(line: HasLineInfo, msg: => Any): Unit = if (logger.isWarnEnabled()) logger.warn(logMsg(line.line, msg))
  protected def error(line: HasLineInfo, msg: => Any): Unit = if (logger.isErrorEnabled()) logger.error(logMsg(line.line, msg))

  protected def debug(msg: => Any): Unit = if (logger.isDebugEnabled()) logger.debug(logMsg(0, msg))
  protected def info(msg: => Any): Unit = if (logger.isInfoEnabled()) logger.info(logMsg(0, msg))
  protected def warn(msg: => Any): Unit = if (logger.isWarnEnabled()) logger.warn(logMsg(0, msg))
  protected def error(msg: => Any): Unit = if (logger.isErrorEnabled()) logger.error(logMsg(0, msg))
}
