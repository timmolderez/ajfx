package be.ac.ua.ansymo.ajfx.util;

import java.util.logging.Formatter;
import java.util.logging.LogRecord;

/**
 * Defines the formatting of log messages..
 * @author Tim Molderez
 */
public class LogFormatter extends Formatter {
	public String format(LogRecord record) {
		StringBuilder sb = new StringBuilder();
		
		return record.getLevel().getLocalizedName() 
				+ ": " + formatMessage(record) + "\n"; 
	}
}
