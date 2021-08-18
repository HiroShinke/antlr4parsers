

package com.github.hiroshinke.cobolsample;

import java.util.regex.Pattern;
import java.util.regex.Matcher;


public class DataItem {

    String file;
    int   level;
    String name;
    String usage;
    String redefines;
    String occurs;
    int    offset;
    String copymem;


    public static DataItem createItem(String level,
				      String name,
				      String pict,
				      String usage,
				      String value,
				      String redefines,
				      String occurs){
	if( pict.isEmpty() ){
	    return new GroupItem(level,name,usage,redefines,occurs);
	}
	else {
	    return new BasicItem(level,name,pict,usage,value,redefines,occurs);
	}

    }
    
    static class GroupItem extends DataItem {

	public GroupItem(String level,
			 String name,
			 String usage,
			 String redefines,
			 String occurs){
	    super(level,name,usage,redefines,occurs);
	}
    }
    
    static class BasicItem extends DataItem {

	String pict;
	String value;
	int    numOfChar;
	int    size;
	
	public BasicItem(String level,
			 String name,
			 String pict,
			 String usage,
			 String value,
			 String redefines,
			 String occurs){
	    super(level,name,usage,redefines,occurs);
	    this.pict = pict;
	    this.value = value;
	    this.size   = calculateSize(usage,pict);
	    this.numOfChar   = pictureCountChar(pict);	
	}
    }
    

    DataItem(String level,
	     String name,
	     String usage,
	     String redefines,
	     String occurs) {

	this.level = Integer.valueOf(level);
	this.name  = name;
	this.usage = usage;
	this.redefines = redefines;
	this.occurs = occurs;
    }

    static int calculateSize(String usage,String pict){

	int cnum =  pictureCountChar(pict);
	    
	if( usage == null || usage.isEmpty() ||
	    usage.equals("DISPLAY") ){
	    return cnum;
	}
	else if( usage.equals("BINARY") ||
		 usage.equals("COMP") ||
		 usage.equals("COMP-4") ||
		 usage.equals("COMP-5") ||
		 usage.equals("COMPUTATIONAL") ||
		 usage.equals("COMPUTATIONAL-4") ||
		 usage.equals("COMPUTATIONAL-5") ){

	    if( cnum <= 4 ){
		return 2;
	    }
	    else if(cnum <= 9 ){
		return 4;
	    }
	    else if(cnum <= 18 ){
		return 8;
	    }
	    else {
		throw new RuntimeException("can't come here");
	    }
	}
	else if( usage.equals("COMP-3") ||
		 usage.equals("COMPUTATIONAL-3") ){
	    return cnum % 2 == 0 ? (cnum+2)/2 : (cnum+1)/2;
	}
	else if( usage.equals("INDEX") ){
	    // TODO : correct calculation
	    return 0;
	}
	else {
	    throw new RuntimeException("not supported yet: usage=" + usage );
	}
	    
    }

    static int pictureCountChar(String pict){

	if( pict == null ){
	    return 0;
	}
	
	int len = pict.length();
	int pos = 0;
	int count = 0;

	while(pos<len){

	    char c = pict.charAt(pos);

	    int unit = 0;
	    int mult = 1;

	    if( c == 'X' || c == '9' || c == '-' || c == '+' ){
		unit = 1;
	    }
	    else if( c == '.' || c == 'S' || c == 'V' || c == ','){
		unit = 0;
	    }
	    pos++;
	    char d = (pos<len) ? pict.charAt(pos) : '\0';
	    if( d == '(' ){
		ParseState<Integer> ret = parseMultiplier(pict,pos);
		pos  = ret.pos;
		mult = ret.value;
	    }

	    count += unit*mult;
	}
	return count;
    }

    static Pattern pattern = Pattern.compile("\\((\\d+)\\)");

    static class ParseState<T> {
	int pos;
	T   value;
	ParseState(int pos,T value){
	    this.pos = pos;
	    this.value = value;
	}
    }
	
    static ParseState<Integer>  parseMultiplier(String pict,int pos){

	Matcher m = pattern.matcher(pict.substring(pos));
	if( m.find() ){
	    String num  = m.group(1);
	    int    size = m.end() - m.start();
	    return new ParseState<Integer>
		(pos + size ,Integer.valueOf(num));
	}
	else {
	    throw new RuntimeException("cant come here");
	}
    }
}
    
