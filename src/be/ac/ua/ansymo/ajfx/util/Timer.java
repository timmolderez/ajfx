package be.ac.ua.ansymo.ajfx.util;

import java.util.Date;

/**
 * Just a simple utility to measure how long something takes
 * @author Tim Molderez
 */
public class Timer {
	long lastMeasurement;
	
	/**
	 * Default constructor; already takes the time
	 */
	public Timer() {
		lastMeasurement = getNow();
	}
	
	/**
	 * Get the amount of time between the last call to getTimePassed() and this one.
	 * Or, if this is the first time you call it, get the time between the constructor and this call.
	 * @return	the amount of time passed, in milliseconds
	 */
	public long getTimePassed() {
		long now = getNow();
		long timePassed = now - lastMeasurement;
		lastMeasurement = now;
		return timePassed;
	}
	
	/*
	 * What time is it now?
	 * @return	the current time in milliseconds (since Jan 1st 1970)
	 */
	private long getNow()	{
		return new Date().getTime();
	}
}
