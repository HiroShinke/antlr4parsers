
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
import org.antlr.v4.runtime.UnbufferedCharStream;

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
import java.nio.charset.Charset;

import java.util.regex.Pattern;
import java.util.regex.Matcher;

import static com.github.hiroshinke.cobolsample.ParserCommon.*;
import static com.github.hiroshinke.antlr4.AntlrUtil.*;

import org.apache.commons.cli.*;

public class App {

    static boolean asTree = true;

    public static void main(String[] args) throws Exception {

	Options opts = new Options();

	Option srcpath = Option.builder("s")
	    .argName("src")
	    .longOpt("src")
	    .hasArg()
	    .type(String.class)
	    .desc("path of src or src directory")
	    .build();

	Option printAsTree = Option.builder("l")
	    .argName("lisp")
	    .longOpt("lisp")
	    .desc("prettyPrint in lisp format")
	    .build();

	Option printInfo = Option.builder("i")
	    .argName("info")
	    .longOpt("info")
	    .desc("print info")
	    .build();

	opts.addOption(srcpath);
	opts.addOption(printAsTree);	
	opts.addOption(printInfo);
	
	CommandLineParser cmdParser = new DefaultParser();
	CommandLine line = cmdParser.parse( opts, args );

	String filePath = line.getOptionValue("s");
	asTree  = ! line.hasOption("l");

	File fileInput = new File(filePath);

	long start0 = System.currentTimeMillis();
	System.err.printf( "process start: %s\n",filePath);

	if( line.hasOption("i") ){
	    doFile(fileInput,App::printInfo);
	} else {
	    doFile(fileInput,App::printTree);
	}

	System.err.printf( "process end: %s, %f s\n",filePath,
			   (System.currentTimeMillis() - start0)/1000.0);
    }

    static void printTree(File file) throws Exception {

	long start = System.currentTimeMillis();
	
	System.err.printf( "file start: %s\n",file.toString());

	CharStream cs = fileToStream(file);
	// CharStream cs = CharStreams.fromFileName(file.toPath().toString());

	printTree(cs);

	System.err.printf( "file end: %s, %f s\n",
			   file, (System.currentTimeMillis() - start)/1000.0);

    }


    static void printTree(CharStream cs) throws Exception {

	VisualBasic6Parser parser = createParser(cs);
	ParseTree tree = parser.startRule();

	if( asTree ){
	    System.out.println(prettyTree(parser,tree));
	} else {
	    System.out.println(tree.toStringTree(parser));
	}
    }

    static CharStream fileToStream(File file) throws Exception {

	InputStreamReader bi = new InputStreamReader(new FileInputStream(file));
	BufferedReader rd = new BufferedReader(bi);
	String line = null;
	StringBuffer buff = new StringBuffer();
	while( (line = rd.readLine()) != null ){
	    buff.append(line);
	    buff.append("\n");
	}
	return new ANTLRInputStream(buff.toString());
    }

    static void printInfo(File file) throws Exception {

	long start = System.currentTimeMillis();

	System.err.printf( "file start: %s\n",file.toString());
	
	CharStream cs = fileToStream(file);
	// CharStream cs = CharStreams.fromFileName(file.toPath().toString());

	VisualBasic6Parser parser = createParser(cs);

	printVarDef(file.toString(), parser);	
	printConstDef(file.toString(), parser);	
	printFuncDef(file.toString(), parser);
	printCallInfo(file.toString(), parser);
	
	System.err.printf( "file end: %s, %f s\n",
			   file, (System.currentTimeMillis() - start)/1000.0);

    }

    static void printCallInfo(String file, VisualBasic6Parser parser) throws Exception {

	parser.reset();
	ParseTree tree = parser.startRule();

	Collection<ParseTree> calls1 = xpathSubTrees
	    (parser,tree,List.of
	     ("//eCS_ProcedureCall/ambiguousIdentifier",
	      "//eCS_MemberProcedureCall/ambiguousIdentifier",
	      "//iCS_B_ProcedureCall/certainIdentifier",
	      "//iCS_B_MemberProcedureCall/ambiguousIdentifier",
	      "//iCS_S_VariableOrProcedureCall/ambiguousIdentifier",
	      "//iCS_S_ProcedureOrArrayCall/ambiguousIdentifier",
	      "//iCS_S_NestedProcedureCall/ambiguousIdentifier"
	      )
	     );
	
	for( ParseTree c : calls1){
	    printOutput( "ProcedureCall", file , c.getText());
	}

    }

    static void printFuncDef(String file, VisualBasic6Parser parser) throws Exception {

	parser.reset();
	ParseTree tree = parser.startRule();

	Collection<ParseTree> defs = xpathSubTrees
	    (parser,tree,List.of
	     ("//functionStmt/ambiguousIdentifier")
	     );

	for( ParseTree c : defs){
	    printOutput( "functionStmt", file , c.getText());
	}

	defs = xpathSubTrees
	    (parser,tree,List.of
	     ("//subStmt/ambiguousIdentifier")
	     );

	for( ParseTree c : defs){
	    printOutput( "subStmt", file , c.getText());
	}

	
    }

    static void printVarDef(String file, VisualBasic6Parser parser) throws Exception {

	parser.reset();
	ParseTree tree = parser.startRule();

	Collection<ParseTree> defs = xpathSubTrees
	    (parser,tree,List.of
	     ("//variableStmt//variableSubStmt",
	      "//letStmt/implicitCallStmt_InStmt/"+
	      "iCS_S_VariableOrProcedureCall/ambiguousIdentifier"
	      ));

	for( ParseTree c : defs){
	    printOutput( "variableStmt", file , c.getText());
	}

    }


    static void printConstDef(String file, VisualBasic6Parser parser) throws Exception {

	parser.reset();
	ParseTree tree = parser.startRule();

	Collection<ParseTree> defs = xpathSubTrees
	    (parser,tree,List.of
	     ("//constStmt/constSubStmt/ambiguousIdentifier"
	      ));

	for( ParseTree c : defs){
	    printOutput( "constStmt", file , c.getText());
	}
    }
    
    static void printOutput(String... strs){
	System.out.println( String.join(",", strs) );
    }
    
    static VisualBasic6Parser createParser(CharStream cs) throws Exception {
    
        VisualBasic6Lexer lexer = new VisualBasic6Lexer(cs); 
        CommonTokenStream tokens = new CommonTokenStream(lexer); 
        VisualBasic6Parser parser = new VisualBasic6Parser(tokens);

	return parser;
    }
}

