
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

import com.github.hiroshinke.cobolpp.CobolPreprocessor;
import static com.github.hiroshinke.cobolsample.AntlrUtil.*;
import static com.github.hiroshinke.cobolsample.ParserCommon.*;

import org.apache.commons.cli.*;


class App {
    
    public static void main(String[] args) throws Exception {


	Options opts = new Options();

	Option srcpath = Option.builder("s")
	    .argName("src")
	    .hasArg()
	    .desc("path of src or src directory")
	    .build();

	Option libpathes = Option.builder("I")
	    .argName("libpath")
	    .hasArgs()
	    .valueSeparator(';')
	    .desc("path to directory including copy members")
	    .build();

	opts.addOption(srcpath);
	opts.addOption(libpathes);
	
	CommandLineParser cmdParser = new DefaultParser();
	CommandLine line = cmdParser.parse( opts, args );

	String filePath = line.getOptionValue("s");
	String[] libpathValue  = line.getOptionValues("I");

	if( filePath != null ){

	    File fileInput = new File(filePath);

	    long start0 = System.currentTimeMillis();
	    System.err.printf( "process start: %s\n",filePath);

	    CobolPreprocessor prep = new CobolPreprocessor(libpathValue);
	    
	    doFile(fileInput,(file) -> {

		    long start = System.currentTimeMillis();

		    System.err.printf( "file start: %s\n",file.toString());

		    InputStream is0 = toSrcStream(new FileInputStream(file));
		    InputStream is  = prep.preprocessStream(is0);

		    Cobol85Parser parser = createParser(is);
		    printCallInfo(file.toString(),parser);
		    printMoveInfo(file.toString(),parser);
		    printDataDescriptionInfo(file.toString(),parser);

		    System.err.printf( "file end: %s, %f s\n",
				       file, (System.currentTimeMillis() - start)/1000.0);
		    
		});

	    System.err.printf( "process end: %s, %f s\n",filePath,
			       (System.currentTimeMillis() - start0)/1000.0);

	} else {

	    Cobol85Parser parser = createParser(System.in);
	    
	    printCallInfo("<stdin>",parser);
	    printMoveInfo("<stdin>",parser);
	    printDataDescriptionInfo("<stdin>",parser);
	}
    }


    public static Cobol85Parser createParser(InputStream is) throws Exception {
    
        ANTLRInputStream input = new ANTLRInputStream(is); 
        Cobol85Lexer lexer = new Cobol85Lexer(input); 
        CommonTokenStream tokens = new CommonTokenStream(lexer); 
        Cobol85Parser parser = new Cobol85Parser(tokens);

	return parser;
    }


    static void printOutput(String... strs){
	System.out.println( String.join(",", strs) );
    }
    
    static void printMoveInfo(String file, Cobol85Parser parser){

	parser.reset();
        ParseTree tree = parser.startRule();
	Collection<ParseTree> moves = xpathSubTrees(parser,tree,"//moveStatement/*");
						    
	for( ParseTree m : moves ){

	    ParseTree from = xpathSubTree
		(
		 parser,
		 m,
		 List.of("//moveToSendingArea","//moveCorrespondingToSendingArea")
		 );

	    Collection<ParseTree> toes = xpathSubTrees(parser,m,"*/identifier");
	    for( ParseTree t : toes ){
		printOutput( "moveStetement",
			     file,
			     prettyString(from),
			     prettyString(t) );
	    }
	}
    }

    static void printCallInfo(String file, Cobol85Parser parser){

	parser.reset();
        ParseTree tree = parser.startRule();

	Collection<ParseTree> calls = xpathSubTrees(parser,tree,"//callStatement");
	for( ParseTree t : calls ){
	    String callName = xpathSubTreeText(parser,t,List.of("*/literal",
								"*/identifier" ));

	    List<String> params = xpathSubTreesTexts(parser,t,"*//callByReference");
	    for(String p : params) {
		printOutput("callStatement",file, callName ,p);
	    }
	}
    }

    static void printDataDescriptionInfo(String file, Cobol85Parser parser){

	parser.reset();
        ParseTree tree = parser.startRule();

	Collection<ParseTree> entries = xpathSubTrees(parser,tree,"//dataDescriptionEntry/*");

	for( ParseTree e : entries ){

	    String level = xpathSubTreeText(parser,e,List.of("*/INTEGERLITERAL",
							     "*/LEVEL_NUMBER_88",
							     "*/LEVEL_NUMBER_66"));
	    String name  = xpathSubTreeText(parser,e,List.of("*/dataName",
							     "*/conditionName"));
	    String pict  = xpathSubTreeText(parser,e,"*//pictureString");
	    String usage = xpathSubTreeText(parser,e,"*/dataUsageClause");
	    String value = xpathSubTreeText(parser,e,"*//dataValueIntervalFrom");
	    String redefines = xpathSubTreeText(parser,e,"*/dataRedefinesClause/dataName");
	    String occurs = xpathSubTreeText(parser,e,"*/dataOccursClause/integerLiteral");
	    
	    printOutput("dataDescription",file,level,name,pict,usage,value,redefines,occurs);
	}
    }

}

