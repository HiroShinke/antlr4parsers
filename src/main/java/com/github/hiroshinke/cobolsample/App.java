
package com.github.hiroshinke.cobolsample;;

/**
 * Hello world!
 *
 */

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.pattern.ParseTreePattern;
import org.antlr.v4.runtime.tree.pattern.ParseTreeMatch;
import org.antlr.v4.runtime.tree.xpath.XPath;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;
import java.util.Collection;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

import java.util.regex.Pattern;
import java.util.regex.Matcher;

import static com.github.hiroshinke.cobolsample.AntlrUtil.*;


class App {


    interface Consumer<T> {
	void accept(T t) throws Exception;
    }
    
    public static void main(String[] args) throws Exception {

        // create a CharStream that reads from standard input

	if( args.length > 0 ){

	    String filePath = args[0];
	    File fileInput = new File(filePath);

	    doFile(fileInput,(file) -> {
		    InputStream is = toSrcStream(new FileInputStream(file));
		    Cobol85Parser parser = createParser(is);
		    printCallInfo(file.toString(),parser);
		    printMoveInfo(file.toString(),parser);
		    printDataDescriptionInfo(file.toString(),parser);
		});

	} else {

	    Cobol85Parser parser = createParser(System.in);
	    
	    printCallInfo("<stdin>",parser);
	    printMoveInfo("<stdin>",parser);
	    printDataDescriptionInfo("<stdin>",parser);
	}
    }

    public static void doFile(File file, Consumer<File> proc) throws Exception {

	if( file.isDirectory() ){
	    doDir(file,proc);
	}
	else {
	    try {
		System.out.println("process file: " + file.toString() );
		proc.accept(file);
	    } catch( Exception e ){
		e.printStackTrace();
		throw e;
	    }
	}
    }

    public static void doDir(File file, Consumer<File> proc) throws Exception {

	for(File f: file.listFiles() ){
	    doFile(f,proc);
	}
    }


    static Pattern pattern = Pattern.compile("^.{6}-\\s+(\"|\')");

    public static InputStream toSrcStream(InputStream is) throws Exception {

	BufferedReader rd = new BufferedReader(new InputStreamReader(is));
	StringBuffer buff = new StringBuffer();

	String line;
	while( (line = rd.readLine()) != null ){
	    if( line.charAt(6) == ' ' ){
		buff.append(line.substring(7,72));
		buff.append('\n');
	    }
	    else if( line.charAt(6) == '-' ){
		int last = buff.length();
		buff.delete(last-1,last);
		Matcher m = pattern.matcher(line);
		if( m.find() ){
		    int e = m.end();
		    buff.append(line.substring(e,72));
		} else {
		    buff.append(line.substring(7,72));
		}
		buff.append('\n');
	    }
	}
	return new ByteArrayInputStream(buff.toString().
					getBytes(StandardCharsets.UTF_8));
    }

    public static Cobol85Parser createParser(InputStream is) throws Exception {
    
        ANTLRInputStream input = new ANTLRInputStream(is); 
        Cobol85Lexer lexer = new Cobol85Lexer(input); 
        CommonTokenStream tokens = new CommonTokenStream(lexer); 
        Cobol85Parser parser = new Cobol85Parser(tokens);

	return parser;
    }

    static void printMoveInfo(String file, Cobol85Parser parser){

	parser.reset();
        ParseTree tree = parser.startRule();
	Collection<ParseTree> moves = XPath.findAll(tree,
						    "//moveStatement/*",
						    parser);
	for( ParseTree m : moves ){

	    String from = xpathSubTreeText
		(
		 parser,m,
		 List.of("//moveToSendingArea","//moveCorrespondingToSendingArea")
		 );

	    Collection<ParseTree> toes = xpathSubTrees(parser,m,"*/identifier");
	    for( ParseTree t : toes ){
		System.out.printf("moveStatement %s,%s,%s\n",
				  file,
				  from ,
				  t.getText() );
	    }
	}
    }

    static void printCallInfo(String file, Cobol85Parser parser){

	parser.reset();
        ParseTree tree = parser.startRule();
	String xpath = "//callStatement"; // get children of blockStatement

	xpathSubTreesDo
	    (
	     parser,tree,xpath,
	     (t) -> {

		 ParseTreePattern pat =
		     patternMatcher(parser,
				    "callStatement",
				    "CALL <foo:literal> <goo:callUsingPhrase>");
		 ParseTreeMatch m = pat.match(t);
		 String callName = m.succeeded() ? m.get("foo").getText() : "";

		 xpathSubTreesDo
		     (
		      parser,t,"*//callByReference",
		      (p) -> {
			  System.out.printf("call : %s, %s, %s\n",
					    file,
					    callName ,
					    p.getText() );
		      });
	     });
    }

    static void printDataDescriptionInfo(String file, Cobol85Parser parser){

	parser.reset();
        ParseTree tree = parser.startRule();

	Collection<ParseTree> entries = XPath.findAll(tree,
						      "//dataDescriptionEntry/*",
						      parser);
	for( ParseTree e : entries ){

	    String level = xpathSubTreeText(parser,e,
					    List.of("*/INTEGERLITERAL",
						    "*/LEVEL_NUMBER_88",
						    "*/LEVEL_NUMBER_66"));
	    String name  = xpathSubTreeText(parser,e,
					    List.of("*/dataName",
						    "*/conditionName"));

	    String pict  = xpathSubTreeText(parser,e,"*//pictureString");
		 
	    String usage = xpathSubTreeText(parser,e,"*/dataUsageClause");

	    String value = xpathSubTreeText(parser,e,"*//dataValueIntervalFrom");

	    System.out.println("dataDescription: " + String.join(",",
								 file,
								 level,
								 name,
								 pict,
								 usage,
								 value));
	}
    }

}

