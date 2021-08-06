

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

class SelectChildrenQuery<T> extends Query<T> {

    Predicate<Path> pred;

    SelectChildrenQuery(Predicate<Path> pred) {
	this.pred = pred;
    }
    
    ArrayList<Path> selectNode(Path p){
	ArrayList<Path> buff = new ArrayList<Path>();
	Expr e = p.tail();
	if( e instanceof ListExpr ){
	    ListExpr le = (ListExpr)e;
	    for( Expr c : le.children) {
		Path cp = p.add(c);
		if( pred.test(cp) ){
		    buff.add(cp);
		}
	    }
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

    static ArrayList<String> findPath(Expr e, String path) {

	Query<String> root    = null;
	Query<String> current = null;
	int i = 0;
	ArrayList<String> spec = parsePath(path);

	while ( i < spec.size() ){
	    String s = spec.get(i);
	    if( s.equals("//") ){
		Query<String> q =
		    new SelectDescendentsQuery<String>
		    (
		     specToPred(spec.get(i+1))
		     );
		if( root == null ){
		    root = q;
		} else if( current != null ){
		    current.continuation = q;
		} 
		current = q;
	    }
	    else if( s.equals("/") ){
		Query<String> q =
		    new SelectChildrenQuery<String>
		    (
		     specToPred(spec.get(i+1))
		     );
		if( root == null ){
		    root = q;
		} else if( current != null ){
		    current.continuation = q;
		}
		current = q;		
	    }
	    i+=2;
	}

	current.continuation = new TerminalQuery( );
	return root.process(new Path(e));
    }

    static ArrayList<String> parsePath (String spec) {

	int pos = 0;
	int length = spec.length();

	ArrayList<String> buff = new ArrayList<String>();
	StringBuffer sb = new StringBuffer();
	while( pos < length ){
	    char c = spec.charAt(pos);
	    if( c == '/' ){
		if( sb.length() > 0){
		    buff.add(sb.toString());
		    sb = new StringBuffer();		    
		}
		char d = pos+1 < length ? spec.charAt(pos+1) : '\0';
		if( d == '/' ){
		    buff.add("//");
		    pos++;
		} else {
		    buff.add("/");
		}
	    }
	    else {
		sb.append(c);
	    }
	    pos++;
	}
	if( sb.length() > 0){
	    buff.add(sb.toString());
	}

	return buff;
    }
    
}

