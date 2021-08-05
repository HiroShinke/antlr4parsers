

package com.github.hiroshinke.antlrsample;


import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.StringJoiner;
import java.util.function.Predicate;



/*
  imutable path 
 */

class Path {

    ArrayList<Expr> path;

    Path() {
	path = new ArrayList<Expr>();
    }

    Path(Expr e) {
	this();
	path.add(e);
    }
    
    private Path(ArrayList<Expr> p){
	path = p;
    }

    Expr tail() {
	return path.get(path.size()-1);
    }

    Path add(Expr e) {
	@SuppressWarnings("unchecked")	
	ArrayList<Expr> p = (ArrayList<Expr>)path.clone();
	p.add(e);
	return new Path(p);
    }

    @Override
    public String toString(){
	StringBuffer buff = new StringBuffer();
	for( Expr e : path ){
	    if( buff.length() == 0 ){
		buff.append( e.getName() );
	    } else {
		buff.append( "/" );
		buff.append( e.getName() );		
	    }
	}
	return buff.toString();
    }
}

abstract class Query<T> {

    abstract ArrayList<Path> selectNode(Path path);    
    Query<T> continuation;    

    Query(){
	this.continuation = null;
    }

    Query(Query<T> continuation){
	this.continuation = continuation;
    }

    ArrayList<T> process(Path path){
	ArrayList<Path> ps = selectNode(path);
	ArrayList<T> buff = new ArrayList<T>();
	for( Path p: ps) {
	    buff.addAll(continuation.process(p));
	}
	return buff;
    }
}

class SelectSelfQuery<T> extends Query<T> {

    Predicate<Path> pred;

    SelectSelfQuery(Predicate<Path> pred) {
	this.pred = pred;
    }
    
    ArrayList<Path> selectNode(Path p){
	ArrayList<Path> buff = new ArrayList<Path>();
	if( pred.test(p) ){
	    buff.add(p);
	}
	return buff;
    }
}

class SelectDescendentsQuery<T> extends Query<T> {

    Predicate<Path> pred;

    SelectDescendentsQuery(Predicate<Path> pred) {
	this.pred = pred;
    }
    
    ArrayList<Path> selectNode(Path p){
	ArrayList<Path> buff = new ArrayList<Path>();
	if( pred.test(p) ){
	    buff.add(p);
	}
	Expr e = p.tail();
	if( e instanceof ListExpr ){
	    ListExpr le = (ListExpr)e;
	    for( Expr c : le.children) {
		buff.addAll(selectNode(p.add(c)));
	    }
	}
	return buff;
    }
}

class TerminalQuery extends Query<String> {

    @Override
    ArrayList<Path> selectNode(Path p){
	ArrayList<Path> buff = new ArrayList<Path>();
	buff.add(p);
	return buff;
    }
    @Override
    ArrayList<String> process(Path p){
	ArrayList<String> buff = new ArrayList<String>();
	buff.add(p.toString());
	return buff;
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
	GrunReader util = new GrunReader(text);

	while(true) {
	    ParseResult r = util.readExpr(0);
	    if (r == null ) break;
	}
    }

    static Predicate<Path> specToPred(String spec) {

	return ( (p) -> {
		Expr e = p.tail();
		if( e.getName().equals(spec) ){
			return true;
		    } else {
			return false;
		    }
		}
	    );
	    
    }

    static ArrayList<String> findTag(Expr e, String spec) {

	Query<String> q = new SelectDescendentsQuery<String>( specToPred(spec) );
	q.continuation = new TerminalQuery( );
	return q.process(new Path(e));
    }

}

