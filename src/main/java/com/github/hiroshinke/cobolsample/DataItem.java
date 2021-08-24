

package com.github.hiroshinke.cobolsample;

import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.List;
import java.util.Deque;
import java.util.function.Consumer;
import java.util.regex.Pattern;
import java.util.regex.Matcher;


public abstract class DataItem {

    String file;
    int   level;
    String levelStr;
    String name;
    String usage;
    String redefines;
    String occurs;
    int    offset;
    String copymem;
    int    size;

    DataItem parent;


    public static DataItem createItem(String file,
				      String level,
				      String name,
				      String pict,
				      String usage,
				      String value,
				      String redefines,
				      String occurs){
	if( pict.isEmpty() ){
	    return new GroupItem(file,level,name,usage,redefines,occurs);
	}
	else {
	    return new BasicItem(file,level,name,pict,usage,value,redefines,occurs);
	}
    }

    
    public static class Stack {

	Deque<DataItem> stack = new ArrayDeque<DataItem>();
	List<DataItem> list = new ArrayList<DataItem>();

	void registerItem(DataItem item) {

	    DataItem lastRemoved = null;
	    while( 0 < stack.size() && item.level <= stack.peekFirst().level ){
		lastRemoved = stack.removeFirst();
		lastRemoved.getSize();
		if( lastRemoved.level == 1 ) {
		    list.add(lastRemoved);
		}
	    }
	    
	    if( stack.size() == 0 ){
		item.offset = 0;
	    }
	    else {
		if( lastRemoved != null ){
		    item.offset = lastRemoved.offset + lastRemoved.getSize();
		}
		else {
		    item.offset = stack.peekFirst().offset;
		}
		stack.peekFirst().add(item);		    
	    }
	    
	    stack.addFirst(item);
	}

	void rewindAll() {
	    while( 0 < stack.size() ){
		DataItem removed = stack.removeFirst();
		removed.getSize();
		if( removed.level == 1 ) {
		    list.add(removed);
		}
	    }
	}

	List<DataItem> getList() { return list; }
    }



    
    static class GroupItem extends DataItem {

	ArrayList<DataItem> children = new ArrayList<DataItem>();

	boolean requireSizeUpdate = true;

	public GroupItem(String file,
			 String level,
			 String name,
			 String usage,
			 String redefines,
			 String occurs){
	    super(file,level,name,usage,redefines,occurs);
	}

	@Override
	DataItem add(DataItem item){
	    children.add(item);
	    item.parent = this;
	    return this;
	}

	@Override
	int getSize(){
	    if( requireSizeUpdate ){
		size = 0;
		for(DataItem i: children){
		    size += i.getSize();
		}
		if( ! occurs.isEmpty() ){
		    size = size * Integer.valueOf(occurs);
		}
		requireSizeUpdate = false;
	    }
	    return size;
	}

	@Override
	String [] makeDescription(){

	    return new String[] {
		"dataDescription",		
		file,
		String.format("%02d",level),		
		name,
		"",
		usage,
		"",
		redefines,
		occurs,
		Integer.toString(offset),
		Integer.toString(size)
	    };


	}

	@Override
	void recursiveDoDataItem( Consumer<DataItem> proc ) {
	    proc.accept(this);
	    for(DataItem c: children){
		c.recursiveDoDataItem(proc);
	    }
	}
    }
    
    static class BasicItem extends DataItem {

	String pict;
	String value;
	int    numOfChar;
	
	public BasicItem(String file,
			 String level,
			 String name,
			 String pict,
			 String usage,
			 String value,
			 String redefines,
			 String occurs){
	    super(file,level,name,usage,redefines,occurs);
	    this.pict = pict;
	    this.value = value;
	    this.size   = calculateSize(usage,pict);
	    int occursNum = occurs.isEmpty() ? 1 : Integer.valueOf(occurs);
	    this.numOfChar   = pictureCountChar(pict);
	}

	@Override
	DataItem add(DataItem item){
	    throw new RuntimeException("cant come here");
	}

	@Override
	int getSize(){
	    return size;
	}

	@Override
	String [] makeDescription(){

	    return new String[] {
		"dataDescription",
		file,
		String.format("%02d",level),
		name,
		pict,
		usage,
		value,
		redefines,
		occurs,
		Integer.toString(offset),		
		Integer.toString(size)
	    };

	}

	@Override
	void recursiveDoDataItem( Consumer<DataItem> proc ) {
	    proc.accept(this);
	}
	
    }
    

    DataItem(String file,
	     String level,
	     String name,
	     String usage,
	     String redefines,
	     String occurs) {
	this.file  = file;
	this.level = Integer.valueOf(level);
	this.levelStr = level;
	this.name  = name;
	this.usage = usage;
	this.redefines = redefines;
	this.occurs = occurs;
    }

    abstract DataItem add(DataItem item);
    abstract int getSize();
    abstract String[] makeDescription();
    abstract void recursiveDoDataItem( Consumer<DataItem> proc );

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
    
