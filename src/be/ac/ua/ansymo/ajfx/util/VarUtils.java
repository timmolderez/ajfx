package be.ac.ua.ansymo.ajfx.util;

import be.ac.ua.ansymo.ajfx.containers.Variable;

public class VarUtils {
	public static Variable create(String var) {
		return null;
	}
	
	public static boolean isGlobal(String var) {
		// If there's a dot in there, it's a static variable..
		return var.indexOf(Character.getNumericValue('.')) != -1;
	}
	
	public static boolean isParameter(String var) {
		// If it starts with a number 
		return Character.isDigit(var.charAt(0));
	}
	
	public static boolean isLocal(String var) {
		return !isGlobal(var) && !isParameter(var);
	}
}
