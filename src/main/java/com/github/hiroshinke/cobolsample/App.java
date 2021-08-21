
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
import java.util.HashMap;
import java.util.Collection;
import java.util.stream.Collectors;
import java.util.Deque;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

import com.github.hiroshinke.cobolpp.CobolPreprocessor;
import static com.github.hiroshinke.cobolsample.AntlrUtil.*;
import static com.github.hiroshinke.cobolsample.ParserCommon.*;

import org.apache.commons.cli.*;


class App {

    static class FileDescription {
	String fileName;
	String assignmentName;
	List<String> recNames;
    }

    public static void main(String[] args) throws Exception {


	Options opts = new Options();

	Option srcpath = Option.builder("s")
	    .argName("src")
	    .longOpt("file")
	    .hasArg()
	    .type(String.class)
	    .desc("path of src or src directory")
	    .build();

	Option libpathes = Option.builder("I")
	    .argName("libpath")
	    .longOpt("libpath")
	    .hasArgs()
	    .type(String.class)
	    .valueSeparator(';')
	    .desc("path to directory including copy members")
	    .build();

	opts.addOption(srcpath);
	opts.addOption(libpathes);
	
	CommandLineParser cmdParser = new DefaultParser();
	CommandLine line = cmdParser.parse( opts, args );

	String filePath = line.getOptionValue("s");
	String[] libpathValue  = line.getOptionValues("I");
	if( libpathValue == null ) {
	    libpathValue = new String[]{};
	};

	if( filePath != null ){

	    File fileInput = new File(filePath);

	    long start0 = System.currentTimeMillis();
	    System.err.printf( "process start: %s\n",filePath);

	    CobolPreprocessor prep = new CobolPreprocessor(libpathValue);
	    doFile(fileInput,file -> parseFile(file,prep));

	    System.err.printf( "process end: %s, %f s\n",filePath,
			       (System.currentTimeMillis() - start0)/1000.0);

	} else {
	    HelpFormatter format = new HelpFormatter();
	    format.printHelp("cobolparser", opts);
	}
    }


    static void parseFile(File file,CobolPreprocessor prep ) throws Exception {

	long start = System.currentTimeMillis();
	
	System.err.printf( "file start: %s\n",file.toString());
	
	InputStream is0 = toSrcStream(new FileInputStream(file));
	InputStream is  = prep.preprocessStream(is0);
	
	Cobol85Parser parser = createParser(is);
	printCallInfo(file.toString(),parser);
	printMoveInfo(file.toString(),parser);
	printDataDescriptionInfo(file.toString(),parser);

	HashMap<String,FileDescription> fileDict
	    = new HashMap<String,FileDescription>();

	printFileControlInfo(file.toString(),parser,fileDict);
	printFileInfo(file.toString(),parser,fileDict);
	printFileIOInfo(file.toString(),parser,fileDict);

	System.err.printf( "file end: %s, %f s\n",
			   file, (System.currentTimeMillis() - start)/1000.0);

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
		(parser,m,List.of("//moveToSendingArea",
				  "//moveCorrespondingToSendingArea"));

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

	DataItem.Stack stack = new DataItem.Stack();

	for( ParseTree e : entries ){

	    String level = xpathSubTreeText(parser,e,List.of("*/INTEGERLITERAL",
							     "*/LEVEL_NUMBER_77",
							     "*/LEVEL_NUMBER_88",
							     "*/LEVEL_NUMBER_66"));
	    String name  = xpathSubTreeText(parser,e,List.of("*/dataName",
							     "*/conditionName"));
	    String pict  = xpathSubTreeText(parser,e,"*//pictureString");
	    String usage = xpathSubTreeText(parser,e,"*/dataUsageClause");
	    if( 5 < usage.length() && usage.substring(0,5).equals("USAGE") ){
		usage = usage.substring(5);
		if( 2 < usage.length() && usage.substring(0,2).equals("IS") ){
		    usage = usage.substring(2);
		}
	    }
	    String value = xpathSubTreeText(parser,e,"*//dataValueIntervalFrom");
	    String redefines = xpathSubTreeText(parser,e,"*/dataRedefinesClause/dataName");
	    String occurs = xpathSubTreeText(parser,e,"*/dataOccursClause/integerLiteral");

	    if( ! level.equals("88") && ! level.equals("66") && ! level.equals("77") ){

		DataItem item = DataItem.createItem(file,
						    level,
						    name,
						    pict,
						    usage,
						    value,
						    redefines,
						    occurs);

		stack.registerItem(item);
	    }
	}

	stack.rewindAll();
	List<DataItem> list = stack.getList();

	for(DataItem d : list){
	    d.recursiveDoDataItem(x -> printOutput(x.makeDescription()));
	}

    }

    
    static void printFileControlInfo(String file,
				     Cobol85Parser parser,
				     HashMap<String,FileDescription> fileDict){

	parser.reset();
        ParseTree tree = parser.startRule();

	Collection<ParseTree> entries = xpathSubTrees(parser,tree,"//fileControlEntry");

	for( ParseTree e : entries ){

	    String fileName   = xpathSubTreeText(parser,e,"//fileName");
	    String assignName = xpathSubTreeText(parser,e,"//assignmentName");

	    FileDescription fd = new FileDescription();
	    fd.fileName = fileName;
	    fd.assignmentName = assignName;
	    fileDict.put(fileName,fd);
	    
	    printOutput("fileControlEntry",file,fileName,assignName);
	}
    }

    static void printFileInfo(String file,
			      Cobol85Parser parser,
			      HashMap<String,FileDescription> fileDict){

	parser.reset();
        ParseTree tree = parser.startRule();

	Collection<ParseTree> entries = xpathSubTrees(parser,tree,"//fileDescriptionEntry");

	for( ParseTree e : entries ){

	    String fileName  = xpathSubTreeText(parser,e,"*/fileName");
	    FileDescription fd = fileDict.get(fileName);

	    Collection<ParseTree> items = xpathSubTrees
		(parser,e,"//dataDescriptionEntry/*");
	    
	    List<String> recNames = items.stream()
		.filter( n -> {
			String level = xpathSubTreeText(parser,n,"*/INTEGERLITERAL");
			return level.equals("01") || level.equals("1");
		    })
		.map( n -> xpathSubTreeText(parser,n,"*/dataName") )
		.collect( Collectors.toList() );

	    fd.recNames = recNames;
		
	    for( String recName : recNames ){
		    printOutput("fileDescription",file,fileName,recName);
	    }
	}
    }
    

    static void printFileIOInfo(String file,
				Cobol85Parser parser,
				HashMap<String,FileDescription> fileDict){

	parser.reset();
        ParseTree tree = parser.startRule();

	HashMap<String,FileDescription> reverseMap =
	    new HashMap<String,FileDescription>();
	
	for(String key: fileDict.keySet()){
	    FileDescription fd = fileDict.get(key);
	    for(String rec: fd.recNames){
		reverseMap.put(rec,fd);
	    }
	}

	Collection<ParseTree> entries = xpathSubTrees(parser,tree,"//readStatement");
	for( ParseTree e : entries ){

	    String fileName = xpathSubTreeText(parser,e,"*/fileName");
	    FileDescription fd = fileDict.get(fileName);
	    for(String r: fd.recNames){
		printOutput("fileIO",file,
			    fd.fileName,fd.assignmentName,r,"I");
	    }
	    String rec2 = xpathSubTreeText
		(parser,e,"//readInto/identifier");

	    if( !rec2.isEmpty() ){	    
		printOutput("fileIO",file,
			    fd.fileName,fd.assignmentName,rec2,"I");
	    }
	}

	entries = xpathSubTrees(parser,tree,"//writeStatement");
	for( ParseTree e : entries ){

	    String rec1 = xpathSubTreeText(parser,e,"*/recordName");
	    FileDescription fd = reverseMap.get(rec1);

	    printOutput("fileIO",file,
			fd.fileName,fd.assignmentName,rec1,"O");

	    String rec2 = xpathSubTreeText
		(parser,e,"//writeFromPhrase/identifier");

	    if( !rec2.isEmpty() ){
		printOutput("fileIO",file,
			    fd.fileName,fd.assignmentName,rec2,"O");
	    }
			    
	}

    }
    
}

