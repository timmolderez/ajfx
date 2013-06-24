package be.ac.ua.ansymo.ajfx.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

/**
 * Reads text files into an array, one entry for each line in the file.
 * 
 * Empty lines and comments (lines prefixed with "//" ) are ignored.
 * Lines starting with ";" are concatenated with the previous line.
 * 
 * @author Tim Molderez
 */
public class TextReader {
	/**
	 * Parse the given file into an array
	 * @param f
	 * @return
	 * @throws IOException 
	 */
	public static ArrayList<String> read(File f) throws IOException {
		ArrayList<String> result = new ArrayList<String>();

		FileReader fr = new FileReader(f);
		BufferedReader br = new BufferedReader(fr);

		String line;
		while((line = br.readLine()) != null) {
			if (line.startsWith(";") && result.size()!=0) {
				int last = result.size()-1;
				result.set(last, result.get(last) + line);
				continue;
			}
			if (!line.equals("") && !line.startsWith("//") ) {
				result.add(line);
			}
		}
		fr.close(); 
		
		return result; 
	}
}