
package com.github.hiroshinke.vbsample;

/**
 * Hello world!
 *
 */

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.antlr.v4.runtime.RuleContext;
import org.antlr.v4.runtime.tree.pattern.ParseTreePattern;
import org.antlr.v4.runtime.tree.pattern.ParseTreeMatch;
import org.antlr.v4.runtime.tree.xpath.XPath;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

import java.util.regex.Pattern;
import java.util.regex.Matcher;

import static com.github.hiroshinke.cobolsample.ParserCommon.*;
import static com.github.hiroshinke.antlr4.AntlrUtil.*;


public class App {


    public static void main(String[] args) throws Exception {

	if( args.length > 0 ){

	    String filePath = args[0];
	    File fileInput = new File(filePath);

	    long start0 = System.currentTimeMillis();
	    System.err.printf( "process start: %s\n",filePath);
	    doFile(fileInput,App::parseFile);
	    System.err.printf( "process end: %s, %f s\n",filePath,
			       (System.currentTimeMillis() - start0)/1000.0);

	} else {

	    throw new IllegalArgumentException();
	}
    }

    static void parseFile(File file) throws Exception {

	long start = System.currentTimeMillis();
	
	System.err.printf( "file start: %s\n",file.toString());

	CharStream cs = CharStreams.fromFileName(file.toPath().toString());
	VisualBasic6Parser parser = createParser(cs);
	ParseTree tree = parser.startRule();
	System.out.println(tree.toStringTree(parser));
	
	System.err.printf( "file end: %s, %f s\n",
			   file, (System.currentTimeMillis() - start)/1000.0);

    }
    
    static VisualBasic6Parser createParser(CharStream cs) throws Exception {
    
        VisualBasic6Lexer lexer = new VisualBasic6Lexer(cs); 
        CommonTokenStream tokens = new CommonTokenStream(lexer); 
        VisualBasic6Parser parser = new VisualBasic6Parser(tokens);

	return parser;
    }
}

