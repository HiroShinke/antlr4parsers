
package com.github.hiroshinke.cobolpp;

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


public class CobolPreprocessor  {


    List<String> libpathes;

    public CobolPreprocessor(String... pathes){
	libpathes = List.of(pathes);
    }
    
    public static void main(String[] args) throws Exception {

	if( args.length > 0 ){

	    String filePath = args[0];
	    File fileInput = new File(filePath);

	    long start0 = System.currentTimeMillis();
	    System.err.printf( "process start: %s\n",filePath);
	    doFile(fileInput,CobolPreprocessor::parseFile);
	    System.err.printf( "process end: %s, %f s\n",filePath,
			       (System.currentTimeMillis() - start0)/1000.0);

	} else {

	    Cobol85PreprocessorParser parser = createParser(System.in);
	    ParseTree tree = parser.startRule();	    
	    System.out.println(tree.toStringTree(parser));	    
	}
    }

    static void parseFile(File file) throws Exception {

	long start = System.currentTimeMillis();
	
	System.err.printf( "file start: %s\n",file.toString());
	
	InputStream is = toSrcStream(new FileInputStream(file));
	Cobol85PreprocessorParser parser = createParser(is);
	ParseTree tree = parser.startRule();
	// System.out.println(tree.toStringTree(parser));
	printTree(parser,tree);
	
	System.err.printf( "file end: %s, %f s\n",
			   file, (System.currentTimeMillis() - start)/1000.0);

    }
    
    public static void printTree(Parser parser, ParseTree tree){

	Collection<ParseTree> subs = xpathSubTrees(parser,tree,List.of("/startRule/*"));
	for( ParseTree s : subs ){
	    if( s instanceof RuleContext ){
		RuleContext rc = (RuleContext)s;
		String ruleName = parser.getRuleNames()[rc.getRuleIndex()];
		if( ruleName.equals("charDataLine") ){
		    System.out.println( srcString(rc,65) );
					
		}
		else if( ruleName.equals("copyStatement") ){
		    System.err.println( srcString((ParseTree)rc,65) );
		    String copymem = xpathSubTreeText(parser,
						      s,
						      "*/copySource");
		    System.err.printf("copy = %s\n",copymem);
		    
		}
		else if( ruleName.equals("replaceOffStatement") ){
		    System.err.println( srcString(rc,65) );
		}
		else if( ruleName.equals("replaceArea") ){
		    System.err.println( srcString(rc,65) );
		}
		else {
		    throw new RuntimeException("unsupportedRule: " + ruleName);
		}
	    }
	    else {
		// System.out.println( s.getText() );
	    }

	}
    }

    public static class SrcText implements Cloneable {

	String  text;
	int     line;
	int     startPos;
	boolean replaced = false;
	
	public SrcText(String text,
		       int line,
		       int startPos){
	    this.text = text;
	    this.line = line;
	    this.startPos = startPos;
	}
	public String getText() { return text; }

	String hexs(String text){
	    byte[] bytes = text.getBytes();
	    ArrayList<String> buff = new ArrayList<String>();
	    for( byte b : bytes ){
		buff.add( String.format("%02x",b) );

	    }
	    return String.join(":",buff);
	}

	@Override
	public String toString() {
	    return String.format("%s,%s,%d,%d",
				 text,hexs(text),line,startPos);
	}

	@Override
	public SrcText clone() {
	    try {
		return (SrcText)super.clone();
	    }
	    catch( CloneNotSupportedException e ){
		throw new AssertionError();
	    }
	}
    }


    public static ArrayList<SrcText> srcTextsFromTree(ParseTree tree){
						 
	ArrayList<TerminalNode> nodes = new ArrayList<TerminalNode>();
	terminalNodeHelper(nodes,tree);
	ArrayList<SrcText> texts = new ArrayList<SrcText>();

	for(TerminalNode n : nodes) {

	    String text = n.getText();
	    if( text.equals("\n") ){
		continue;
	    }
	    texts.add( new SrcText(text,
				   n.getSymbol().getLine(),
				   n.getSymbol().getCharPositionInLine()));
	}
	return texts;
    }
    

    /**

       @param fileWidth fill line with space to fixed length of fillWidth.
              0 means no filling.
     */

    public static String srcFromSrcTexts(ArrayList<SrcText> texts, int fillWidth) {


	int line0   = 0;
	int pos0    = 0;

	StringBuffer buff = new StringBuffer();

	for(SrcText n : texts) {

	    // TODO: check is this good ?
	    if( n.getText().equals("\n") ){
		continue;
	    }

	    // line, pos start from 1,0
	    int line = n.line;
	    int pos  = n.startPos;

	    // System.out.printf("line,pos,line0,pos0=%d,%d,%d,%d\n",
	    // 		          line,pos,line0,pos0);

	    if( line != line0 ){
		
		if( 1 <= line0 ){
		    char lastChar = buff.charAt(buff.length()-1);
		    if( lastChar != '\n' ){
			if( pos0 < fillWidth ){
			    buff.append(nchar(' ',fillWidth -pos0));
			}
			buff.append('\n');
		    }
		}
		buff.append(nchar(' ',pos));
		line0 = line;
	    }
	    else if( pos0 < pos ){
		buff.append(nchar(' ',pos - pos0));
	    }
	    else {
		pos = pos0;
	    }
	    
	    String text = n.getText();
	    buff.append(text);
	    pos0 = pos + text.length();
	}
	if( pos0 < fillWidth ){
	    buff.append(nchar(' ',fillWidth - pos0));
	}

	return buff.toString();
    }


    public static class ReplaceSpec {

	ArrayList<SrcText> from;
	ArrayList<SrcText> to;

	public ReplaceSpec(ArrayList<SrcText> from,
			   ArrayList<SrcText> to){
	    this.from = from ;
	    this.to   = to;
	}
	
	public String toString() {
	    StringBuffer buff = new StringBuffer();
	    buff.append("ReplaceSpec {");
	    buff.append("from=");
	    buff.append(String.join(",",
				    from.stream()
				    .map(SrcText::toString)
				    .collect(Collectors.toList())));
	    buff.append(";");
	    buff.append("to=");
	    buff.append(String.join(",",
				    to.stream()
				    .map(SrcText::toString)
				    .collect(Collectors.toList())));
	    buff.append(";");
	    buff.append("}");	    
	    return buff.toString();
	}
    }

    /**

      */

    static class ReplaceState {
	int pos;
	ArrayList<SrcText> result;
	ReplaceState(int pos, ArrayList<SrcText> result){
	    this.pos = pos;
	    this.result = result;
	}
    }

    static ReplaceState tryReplacements(ArrayList<SrcText> texts,
					int pos,
					List<ReplaceSpec> replaces){
	for(ReplaceSpec r: replaces){
	    ReplaceState n = tryReplacement(texts,pos,r);
	    if( n != null ){
		return n;
	    }
	}
	return null;
    }

    static ReplaceState tryReplacement(ArrayList<SrcText> texts,
				       int pos,
				       ReplaceSpec replace){
	int i = 0;
	ArrayList<SrcText> from = replace.from;
	while( i < from.size() && pos + i  < texts.size() ){
	    SrcText src = texts.get(pos+i);
	    if( src.getText().equals(from.get(i).getText()) ){
		i++;
	    } else {
		return null;
	    }
	}
	if( i < from.size() ){
	    return null;
	}
	return doReplace(texts,pos,replace);
    }
    

    static ReplaceState doReplace(ArrayList<SrcText> texts,
				  int pos,
				  ReplaceSpec replace){

	ArrayList<SrcText> ret  = new ArrayList<SrcText>();
	ret.addAll(texts.subList(0,pos));

	ArrayList<SrcText> from = replace.from;
	ArrayList<SrcText> to  = new ArrayList<SrcText>
	    (
	     replace.to.stream()
	     .map( t -> t.clone() ).collect(Collectors.toList())
	     );

	if( 0 < to.size() ){
	
	    SrcText t1 = to.get(0);
	    SrcText t2 = texts.get(pos);
	    
	    int lineDiff1     = t2.line - t1.line;
	    int startPosDiff1 = t2.startPos - t1.startPos ;
	    
	    for(SrcText t: to){
		t.replaced = true;
		t.line = t.line + lineDiff1;
		t.startPos  = t.startPos + startPosDiff1;
	    }
	    ret.addAll(pos,to);
	}
	    
	if( pos + from.size() < texts.size() ){

	    ArrayList<SrcText> subAfter  = new ArrayList<SrcText>
		(
		 texts.subList(pos + from.size(), texts.size()).stream()
		 .map( t -> t.clone() ).collect(Collectors.toList())
		 );

	    if( 0 < to.size() ){
	    
		SrcText t3 = to.get(to.size()-1);
		SrcText t4 = texts.get(pos + from.size() -1);
		
		int lineDiff2 = t3.line - t4.line;
		int startPosDiff2 =
		    t3.startPos + t3.text.length()
		    - t4.startPos - t4.text.length();
		
		for(SrcText t: subAfter){
		    t.replaced = true;
		    t.line = t.line + lineDiff2;
		    t.startPos  = t.startPos + startPosDiff2;
		}
	    }
	    ret.addAll(subAfter);
	}

	return new ReplaceState(pos + to.size(),ret);
    }

    
    public static ArrayList<SrcText> applyReplacements(ArrayList<SrcText> texts,	
						       List<ReplaceSpec> replaces){
	int pos = 0;
	while( pos < texts.size() ){
	    ReplaceState state = tryReplacements(texts,pos,replaces);
	    if( state != null ){
		pos = state.pos;
		texts = state.result;
	    } else {
		pos++;
	    }
	}
	return texts;
    }


    static ParseTree getReplacement(Parser parser,ParseTree t) {

	ParseTree a = xpathSubTree(parser,t,List.of("*/literal",
						    "*/cobolWord",
						    "*/charDataLine"));
	if( a != null ){
	    return a;
	}
	a = xpathSubTree(parser,t,"*/pseudoText/charData");

	if( a != null ){
	    return a;
	} else {
	    return null;
	}
    }

    public static ReplaceSpec createReplaceSpec(ParseTree from,
						ParseTree to ){
	
	return new ReplaceSpec(srcTextsFromTree(from),
			       to != null ?
			       srcTextsFromTree(to) : new ArrayList<SrcText>() );
    }

    public static ReplaceSpec nullReplaceSpec(){
	return new ReplaceSpec(new ArrayList<SrcText>(),
			       new ArrayList<SrcText>() );
    }
    
    public InputStream preprocessStream(InputStream is) throws Exception {
	return preprocessStream(is,List.of());
    }

    public List<ReplaceSpec> replaceSpecList(Parser parser, ParseTree s) {
					 
	Collection<ParseTree> replaceClauses = xpathSubTrees(parser,s,"*//replaceClause");
	    
	List<ReplaceSpec> replaceSpec =
	    replaceClauses.stream()
	    .map( t ->
		  {
		      ParseTree a = getReplacement
			  (parser,xpathSubTree(parser,t,"*/replaceable"));
		      ParseTree b = getReplacement
			  (parser,xpathSubTree(parser,t,"*/replacement"));
		      return createReplaceSpec(a,b);
		  })
	    .collect(Collectors.toList());

	return replaceSpec;
    }


    public InputStream preprocessStream(InputStream is,
					List<ReplaceSpec> replacement ) throws Exception {

	return preprocessStream2(preprocessStream1(is,replacement));
    }

    /**
       process copyStatements to expand.

     */
    InputStream preprocessStream1(InputStream is,
				  List<ReplaceSpec> replacement )
	throws Exception {
	
	Cobol85PreprocessorParser parser = createParser(is);
	ParseTree tree = parser.startRule();

	Collection<ParseTree> subs = xpathSubTrees(parser,
						   tree,
						   "/startRule/*");
	StringBuffer buff = new StringBuffer();
	
	for( ParseTree s : subs ){

	    if( s instanceof RuleContext ){

		RuleContext rc = (RuleContext)s;
		String ruleName = parser.getRuleNames()[rc.getRuleIndex()];

		if( ruleName.equals("charDataLine") ||
		    ruleName.equals("replaceOffStatement") ){
		    
		    appendAllTexts(buff,s,replacement);

		}
		else if( ruleName.equals("copyStatement") ){

		    System.err.println( "copyStatement expanded");
		    System.err.println( srcString(rc,65) );
		    String copymem = xpathSubTreeText(parser,s,"*/copySource");
		    List<ReplaceSpec> replaceSpec = replaceSpecList(parser,s);
		    System.err.println( "replaceSpec=" + replaceSpec.toString() );
		    buff.append
			(stringFromStream
			  (
			   preprocessStream1
			   (
			    streamFromCopymem(copymem,replaceSpec),
			    replacement
			    )
			   )
			 );
		}
		else if( ruleName.equals("replaceArea") ){

		    Collection<ParseTree> area  = xpathSubTrees(parser,s,"*/*");
		    
		    for(ParseTree e: area){

			if( e instanceof RuleContext ){

			    RuleContext rc2 = (RuleContext)e;
			    String ruleName2 = parser
				.getRuleNames()[rc2.getRuleIndex()];

			    if( ruleName2.equals("copyStatement") ){
				System.err.println( "copyStatement expanded");
				System.err.println( srcString(rc2,65) );
				String copymem = xpathSubTreeText(parser,e,"*/copySource");
				List<ReplaceSpec> replaceSpec2 = replaceSpecList(parser,e);
				buff.append
				    (stringFromStream
				     (
				      preprocessStream1
				      (
				       streamFromCopymem(copymem,replaceSpec2),
				       replacement
				       )
				      )
				     );
			    } else {
				appendAllTexts(buff,e,replacement);
			    }
			}
		    }
		}
		else {
		    throw new RuntimeException("unsupportedRule: " + ruleName);
		}
	    }
	    else {
		// buff.append(srcString(s,65)); 
	    }
	}
	return new ByteArrayInputStream(buff.toString().
					getBytes(StandardCharsets.UTF_8));
    }


    /**
       process replaceStatements after copy expansion done

     */


    public InputStream preprocessStream2(InputStream is) throws Exception {


	Cobol85PreprocessorParser parser = createParser(is);
	ParseTree tree = parser.startRule();

	Collection<ParseTree> subs = xpathSubTrees(parser,
						   tree,
						   "/startRule/*");
	List<ReplaceSpec> replaceSpec = List.of();
	StringBuffer buff = new StringBuffer();
	
	for( ParseTree s : subs ){

	    if( s instanceof RuleContext ){

		RuleContext rc = (RuleContext)s;
		String ruleName = parser.getRuleNames()[rc.getRuleIndex()];

		if( ruleName.equals("charDataLine") ){
		    appendAllTexts(buff,s,replaceSpec);

		}
		else if( ruleName.equals("copyStatement") ){

		    System.err.println( "copyStatement remained not expanded");
		    System.err.println( srcString(rc,65) );
		    appendAllTexts(buff,s,replaceSpec);
		}
		else if( ruleName.equals("replaceOffStatement") ){
		    replaceSpec = List.of();
		}
		else if( ruleName.equals("replaceArea") ){

		    System.err.println( "replaceArea: ");
		    System.err.println( srcString(rc,65) );

		    Collection<ParseTree> area  = xpathSubTrees(parser,s,"*/*");
		    
		    for(ParseTree e: area){

			if( e instanceof RuleContext ){
			    RuleContext rc2 = (RuleContext)e;
			    String ruleName2 = parser
				.getRuleNames()[rc2.getRuleIndex()];

			    if( ruleName2.equals("replaceByStatement")){
				replaceSpec = replaceSpecList(parser,e);
			    }
			    else if( ruleName2.equals("copyStatement") ){
				System.err.println( "copyStatement remained not expanded");
				System.err.println( srcString(rc2,65) );
				appendAllTexts(buff,e,replaceSpec);
			    }
			    else if( ruleName2.equals("replaceOffStatement") ){
				replaceSpec = List.of();
			    } else {
				appendAllTexts(buff,e,replaceSpec);
			    }
			}
		    }
		}
		else {
		    throw new RuntimeException("unsupportedRule: " + ruleName);
		}
	    }
	    else {
		// buff.append(srcString(s,65)); 
	    }
	}
	return new ByteArrayInputStream(buff.toString().
					getBytes(StandardCharsets.UTF_8));
    }



    void appendAllTexts(StringBuffer buff,
			ParseTree tree,
			List<ReplaceSpec> replacement) {
	ArrayList<SrcText> texts = srcTextsFromTree(tree);
	texts = applyReplacements(texts,replacement);
	if( 0 < texts.size() ){
	    buff.append(srcFromSrcTexts(texts,65));
	    buff.append('\n');
	}
    }
    
    
    static String stripQuote(String literal){
	return literal.substring(1,literal.length()-1);
    }
    
    File findFile(String copymem) throws Exception {

	if( copymem.charAt(0) == '\'' || copymem.charAt(0) == '"'  ){

	    copymem = stripQuote(copymem);
	    for(String path: libpathes){
		File file = new File(path, copymem);
		if( file.exists() ){
		    return file;
		}
	    }

	} else {

	    for(String ext: List.of("", ".cbl", ".txt")){
		for(String path: libpathes){
		    File file = new File(path, copymem + ext);
		    if( file.exists() ){
			return file;
		    }
		}
	    }
	}
	return null;
    }


    InputStream streamFromCopymem(String copymem,
				  List<ReplaceSpec> replaceSpec)
	throws Exception {

	File lib = findFile(copymem);
	if( lib == null) {
	    throw new AssertionError("copymem not found: " + copymem);
	}
	System.err.println("lib=" + lib.getPath());
	
	InputStream is0 = toSrcStream(new FileInputStream(lib));
	return preprocessStream1(is0,replaceSpec);
    }

    String stringFromStream(InputStream is) throws Exception {

	StringBuffer buff = new StringBuffer();

	BufferedReader rd =
	    new BufferedReader
	    ( new InputStreamReader(is) );

	String line = null;
	while( (line = rd.readLine()) != null ){
	    buff.append(line);
	    buff.append('\n');
	}

	//System.err.println("stringFromStream: buff=");
	//System.err.println(buff.toString());
	
	return buff.toString();
    }

    
    void processCopySentence(String copymem,
			     StringBuffer buff,
			     List<ReplaceSpec> replaceSpec) throws Exception {

	BufferedReader rd =
	    new BufferedReader
	    ( new InputStreamReader( streamFromCopymem(copymem,replaceSpec) ) );

	String line = null;
	while( (line = rd.readLine()) != null ){
	    buff.append(line);
	    buff.append('\n');
	}
    }

    static Cobol85PreprocessorParser createParser(InputStream is) throws Exception {
    
        ANTLRInputStream input = new ANTLRInputStream(is); 
        Cobol85PreprocessorLexer lexer = new Cobol85PreprocessorLexer(input); 
        CommonTokenStream tokens = new CommonTokenStream(lexer); 
        Cobol85PreprocessorParser parser = new Cobol85PreprocessorParser(tokens);

	return parser;
    }
}

