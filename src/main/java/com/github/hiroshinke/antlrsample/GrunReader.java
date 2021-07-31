

package com.github.hiroshinke.antlrsample;



class ParseResult {
    Expr e;
    int  pos;
    ParseResult(Expr e,int pos) {
	this.e = e;
	this.pos = pos;
    }
}

public class GrunReader {

    String text;

    public GrunReader(String text){
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
	
	ParseResult r = checkOpen(pos);
	
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
	    char d = pos+1<length ? text.charAt(pos+1) : '\0';
	    if( c != ' ' && c != '(' && c != ')' ){
		buff.append(c);
		pos++;
	    } else if( c == '(' && d == ' ' ){
		buff.append(c);
		pos+=2;
		break;
	    } else if( c == ')' ){
		char b = pos > 0 ? text.charAt(pos-1) : '\0';
		if( b == ' ' ){
		    buff.append(c);
		    pos++;
		} 
		break;
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
	if( pos < text.length() && text.charAt(pos) == c ){
	    return new ParseResult(null,pos+1);
	}
	else {
	    return null;
	}
    }

    ParseResult checkOpen(int pos) {

	int length = text.length();
	
	char c = text.charAt(pos);
	char d = pos+1<length ? text.charAt(pos+1) : '\0';

	if( c == '(' && d != ' '){
	    return new ParseResult(null,pos+1);
	}
	else {
	    return null;
	}
    }

}

