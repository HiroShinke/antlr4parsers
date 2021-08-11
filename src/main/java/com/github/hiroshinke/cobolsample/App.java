
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

import static com.github.hiroshinke.cobolsample.AntlrUtil.*;


class App {

    public static void main(String[] args) throws Exception {
        // create a CharStream that reads from standard input

	InputStream is = System.in;

	if( args.length > 0 ){
	    String filePath = args[0];
	    File fileInput = new File(filePath);
	    is = new FileInputStream(fileInput);
	}

	Cobol85Parser parser = createParser(is);
	// System.out.println(tree.toStringTree(parser));

	printCallInfo(parser);
	printMoveInfo(parser);
	printDataDescriptionInfo(parser);
    }

    public static Cobol85Parser createParser(InputStream is) throws Exception {
    
        ANTLRInputStream input = new ANTLRInputStream(is); 
        Cobol85Lexer lexer = new Cobol85Lexer(input); 
        CommonTokenStream tokens = new CommonTokenStream(lexer); 
        Cobol85Parser parser = new Cobol85Parser(tokens);

	return parser;
    }

    static void printMoveInfo(Cobol85Parser parser){

	parser.reset();
        ParseTree tree = parser.startRule();

	String xpath = "//moveStatement"; // get children of blockStatement
	String treePattern = "MOVE <foo:identifier> TO <goo:identifier>";
	ParseTreePattern p =
	    parser.compileParseTreePattern(
			treePattern,   
			Cobol85Parser.RULE_moveStatement
					  );
	List<ParseTreeMatch> matches = p.findAll(tree, xpath);
	System.out.println(matches);
	for( ParseTreeMatch m : matches ){
	    String from = m.get("foo").getText();
	    String to = m.get("goo").getText();
	    System.out.printf("move : %s -> to : %s\n", from ,to );

	}

    }

    static void printCallInfo(Cobol85Parser parser){

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
			  System.out.printf("call : %s using : %s\n",
					    callName ,
					    p.getText() );
		      });
	     });
    }

    static void printDataDescriptionInfo(Cobol85Parser parser){

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
								 level,
								 name,
								 pict,
								 usage,
								 value));
	}
    }

}

