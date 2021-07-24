

package com.github.hiroshinke.antlrsample;


import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
    
class Expr { }
class ListExpr extends Expr { }
class AtomExpr extends Expr { }

class ParseResult {
    Expr e;
    int  pos;
    ParseResult(Expr e,int pos) {
	this.e = e;
	this.pos = pos;
    }
}

public class GrunUtil {

    String text;

    public static void main(String[] args) throws Exception {

        // Read Grun LISP style output for Analyse it

        String filePath = args[0];
        File fileInput = new File(filePath);
        FileInputStream fileInputStream = new FileInputStream(fileInput);

	String text = new String(fileInputStream.readAllBytes(),
				 StandardCharsets.UTF_8);
	GrunUtil util = new GrunUtil(text);

	while(true) {
	    ParseResult r = util.readExpr(0);
	    if (r == null ) break;
	}
    }

    public GrunUtil(String text){
	this.text = text;
    }

    
    public ParseResult readExpr(int pos){

	ParseResult l = readList(pos);
	if(  l != null ){
	    return l;
	}
	else {
	    ParseResult a = readAtom(pos);
	    if( a != null ) return a;
	    else {
		return null;
	    }
	}
    }

    public ParseResult readList(int pos){

	pos = skipWS(pos);
	ParseResult r = checkCharAt(pos,'(');
	if( r != null ){
	    pos = r.pos;
	    while( true ){
		ParseResult r2 = readExpr(pos);
		if( r2 != null ){
		    Expr e = r2.e;
		    pos = r2.pos;
		}
	    }
	}
	else {
	    return null;
	}
    }

    public ParseResult readAtom(int pos){
	return null;
    }
    
    int skipWS(int pos){

	while( true ){
	    char c = text.charAt(pos);
	    if( c != ' ' || text.length() < pos) {
		break;
	    }
	    pos++;
	}
	return pos;
    }

    ParseResult checkCharAt(int pos,char c) {
	if( text.charAt(pos) == c ){
	    return new ParseResult(null,pos+1);
	}
	else {
	    return null;
	}
    }

    
}

