

package com.github.hiroshinke.antlrsample;


import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.StringJoiner;
    
class Expr { }
class ListExpr extends Expr {

    ArrayList<Expr> children = new ArrayList<Expr>();
    
    void addElement( Expr e ){
	    children.add(e);
    }

    public boolean equals(Object e){

	if( e instanceof ListExpr ){
	    ListExpr l = (ListExpr)e;
	    return children.equals(l.children);
	} else {
	    return false;
	}
    }

    public String toString(){

	ArrayList<String> strs = new ArrayList<String>();
	for(Expr e : children){
	    strs.add(e.toString());
	}

	StringBuffer buff = new StringBuffer();
	buff.append('(');
	buff.append( String.join(" ",strs) );
	buff.append(')');
	return buff.toString();
    }
}

class AtomExpr extends Expr {
    String str;
    AtomExpr(String str){
	this.str = str;
    }
    public boolean equals(Object e){
	if( e instanceof AtomExpr ){
	    AtomExpr a = (AtomExpr)e;
	    return str.equals(a.str);
	} else {
	    return false;
	}
    }
    public String toString() {
	return str;
    }
}

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
	int length = text.length();
	
	ParseResult r = checkCharAt(pos,'(');
	if( r != null ){
	    pos = r.pos;
	    ListExpr l = new ListExpr();
	    while( pos < length ){
		ParseResult r2 = readExpr(pos);
		if( r2 != null ){
		    Expr e = r2.e;
		    l.addElement(e);
		    pos = r2.pos;
		} else {
		    break;
		}
	    }
	    r = checkCharAt(pos,')');
	    if( r != null )
		return new ParseResult(l,r.pos);
	    else
		return null;
	}
	else {
	    return null;
	}
    }

    public ParseResult readAtom(int pos){

	pos = skipWS(pos);
	int length = text.length();

	StringBuffer buff = new StringBuffer();

	while( pos < length ){
	    char c = text.charAt(pos);
	    if( c != ' ' && c != ')' ){
		buff.append(c);
		pos++;
	    } else {
		break;
	    }
	}

	if( buff.length() > 0 ){
	    return new ParseResult( new AtomExpr(buff.toString()),
				    pos );
	} else {
	    return null;
	}
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

