

package com.github.hiroshinke.antlrsample;


import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.StringJoiner;
    

public class GrunUtil {

    String text;

    public static void main(String[] args) throws Exception {

        // Read Grun LISP style output for Analyse it

        String filePath = args[0];
        File fileInput = new File(filePath);
        FileInputStream fileInputStream = new FileInputStream(fileInput);

	String text = new String(fileInputStream.readAllBytes(),
				 StandardCharsets.UTF_8);
	GrunReader util = new GrunReader(text);

	while(true) {
	    ParseResult r = util.readExpr(0);
	    if (r == null ) break;
	}
    }

}

