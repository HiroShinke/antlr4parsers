
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

import java.util.function.Function;
import java.util.function.Consumer;


class App {

    public static void main(String[] args) throws Exception {
        // create a CharStream that reads from standard input

	InputStream is = System.in;

	if( args.length > 0 ){
	    String filePath = args[0];
	    File fileInput = new File(filePath);
	    is = new FileInputStream(fileInput);
	} 
	    
        ANTLRInputStream input = new ANTLRInputStream(is); 
        Cobol85Lexer lexer = new Cobol85Lexer(input); 
        CommonTokenStream tokens = new CommonTokenStream(lexer); 
        Cobol85Parser parser = new Cobol85Parser(tokens);

	// System.out.println(tree.toStringTree(parser));
	// printMoveInfo(parser);
	printCallInfo(parser);
	printMoveInfo(parser);
	printDataDescriptionInfo(parser);
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
	     tree,xpath,parser,
	     (t) -> {

		 ParseTreePattern pat =
		     patternMatcher(parser,
				    "callStatement",
				    "CALL <foo:literal> <goo:callUsingPhrase>");
		 ParseTreeMatch m = pat.match(t);
		 String callName = m.succeeded() ? m.get("foo").getText() : "";

		 xpathSubTreesDo
		     (
		      t,"*//callByReference",parser,
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

	String xpath = "//dataDescriptionEntry/*";
	String xpathLevel = "*/INTEGERLITERAL";
	String xpathLevel66 = "*/LEVEL_NUMBER_66";
	String xpathLevel88 = "*/LEVEL_NUMBER_88";
	String xpathName    = "*/dataName";
	String xpathConditionName = "*/conditionName";
	String xpathPicture = "*/dataPictureClause";
	String xpathUsage   = "*/dataUsageClause";	
	String xpathValue   = "*/dataValueClause";	
	
	Collection<ParseTree> entries = XPath.findAll(tree,xpath,parser);
	for( ParseTree e : entries ){

	    String level = xpathSubTreeText(e,xpathLevel,parser);
	    if( level.isEmpty() ){
		level = xpathSubTreeText(e,xpathLevel88,parser);
	    }
	    if( level.isEmpty() ){
		level = xpathSubTreeText(e,xpathLevel66,parser);
	    }
	    String name  = xpathSubTreeText(e,xpathName,parser);
	    if( name.isEmpty() ){
		name  = xpathSubTreeText(e,xpathConditionName,parser);		
	    }
	    
	    String pict  =
		xpathSubTreesCont
		(e,xpathPicture,parser,
		 (subs) -> {
		    for(ParseTree t: subs) {
			ParseTreePattern pat = patternMatcher(parser,
							      "dataPictureClause",
							      "<PIC> <foo:pictureString>");
			ParseTreeMatch m = pat.match(t);
			if( m.succeeded() ){
			    return m.get("foo").getText();
			} 
		    }
		    return "";
		});
		 
	    String usage  = xpathSubTreeText(e,xpathUsage,parser);

	    String value = 
		xpathSubTreesCont
		(e,xpathValue,parser,
		 (subs) -> {
		    for(ParseTree t: subs) {
			ParseTreePattern pat =
			    patternMatcher(parser,
					   "dataValueClause",
					   "<VALUE> <foo:dataValueIntervalFrom>");
			ParseTreeMatch m = pat.match(t);
			if( m.succeeded() ){
			    return m.get("foo").getText();
			} 
		    }
		    return "";
		});

	    System.out.println("dataDescription: " + String.join(",",
								 level,
								 name,
								 pict,
								 usage,
								 value));
	}
    }

    static ParseTree xpathSubTree(ParseTree tree,
				  String xpath,
				  Parser parser) {
	
	Collection<ParseTree> subs = XPath.findAll(tree,xpath,parser);
	for (ParseTree s: subs) {
	    return s;
	}
	return null;
    }


    static String xpathSubTreeText(ParseTree tree,
				   String xpath,
				   Parser parser) {
	
	ParseTree ret = xpathSubTree(tree,xpath,parser);
	if( ret != null ){
	    return ret.getText();
	} else {
	    return "";
	}
    }

    static <T> T xpathSubTreesCont(ParseTree tree,
				   String xpath,
				   Parser parser,
				   Function<Collection<ParseTree>,T> cont) {

	Collection<ParseTree> subs = XPath.findAll(tree,xpath,parser);
	return cont.apply(subs);
    }

    static <T> void xpathSubTreesDo(ParseTree tree,
				    String xpath,
				    Parser parser,
				    Consumer<ParseTree> cont) {
	
	Collection<ParseTree> subs = XPath.findAll(tree,xpath,parser);
	for( ParseTree t : subs ){
	    cont.accept(t);
	}
    }
    

    static ParseTreePattern patternMatcher(Parser parser,
					   String ruleName,
					   String pattern){
	return parser.compileParseTreePattern(
	    pattern,
	    parser.getRuleIndex(ruleName));
    }


    static List<String> prettyStringHelper(ParseTree tree) {

	ArrayList<String> buff = new ArrayList<String>();
	
	int n = tree.getChildCount();
	if( n == 0 ){
	    buff.add( tree.getText() );
	}
	else {
	    for(int i=0; i<n; i++){
		buff.addAll(prettyStringHelper(tree.getChild(i)));
	    }
	}
	return buff;
    }


    static String prettyString(ParseTree tree) {

	List<String> buff = prettyStringHelper(tree);
	return String.join(" ",buff);
    }
    
}

