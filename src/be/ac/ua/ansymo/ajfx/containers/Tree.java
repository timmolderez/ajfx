package be.ac.ua.ansymo.ajfx.containers;

import java.util.ArrayList;
import java.util.List;

/**
 * Basic implementation of a generic tree; a tree node can have a variable number of children.
 * @author Tim Molderez
 *
 * @param <E>	type of a node's contents
 */
public class Tree<E> {
	private E contents;
	private List<Tree<E>> children;
	private Tree<E> parent;

	/**
	 * Constructor
	 * @param contents
	 */
	public Tree(E contents) {
		this.contents=contents;
		children=new ArrayList<Tree<E>>();
		parent = null;
	}
	
	public Tree(E contents, Tree<E> parent) {
		this(contents);
		this.parent=parent;
	}
	
	/**
	 * Adds a new child node
	 * @param contents	contents of the new node
	 * @return	the child node that was added
	 */
	public Tree<E> addChild(E contents) {
		Tree<E> node = new Tree<E>(contents,this);
		children.add(node);
		return node;
	}
	
	/**
	 * Adds a subtree as a child
	 */
	public void addChild(Tree<E> subtree) {
		children.add(subtree);
	}
	
	/**
	 * Remove all children
	 */
	public void removeChildren() {
		children = new ArrayList<Tree<E>>();
	}
	
	/**
	 * Get the contents of this node
	 * @return
	 */
	public E getContents() {
		return contents;
	}
	
	/**
	 * Get all children of this node
	 * @return
	 */
	public List<Tree<E>> getChildren() {
		return children;
	}
	
	public int getNumberOfChildren() {
		return children.size();
	}
	
	public String toString() {
		return toString(0);
	}
	
	private String toString(int tabs) {
		String str = "";
		
		for(int i=0;i<tabs;i++) {
			str +=" ";
		}
		
		str+=contents.toString() + "\n";
		
		for(Tree<E> child: children) {
			str+=child.toString(tabs+1);
		}
		
		return str;
	}
	
	public Tree<E> getParent() {
		return parent;
	}
	
	public void setParent(Tree<E> p) {
		parent=p;
	}
	
	/**
	 * Go find all nodes that have a certain contents
	 * @param contents
	 * @return
	 */
	public ArrayList<Tree<E>> findNodes(E contents, Filter<E> filter) {
		ArrayList<Tree<E>> found = new ArrayList<Tree<E>>();
		for(Tree<E> child: children) {
			if(filter.matches(contents, child.getContents())) {
				found.add(child);
			}
			found.addAll(child.findNodes(contents, filter));
		}
		return found;
	}
	
	/**
	 * Delete a node, then keep deleting its ancestor nodes if they would only lead to this leaf.
	 * If the node is not part of this tree, nothing happens.
	 * @param node
	 */
	public void removeDanglingPath(Tree<E> node) {
		ArrayList<Tree<E>> path = getPathFromNodeToRoot(node);
		for (Tree<E> tree : path) {
			if (tree.getNumberOfChildren() != 0) {
				tree.removeChildren();
				break;
			}
		}
	}
	
	public ArrayList<Tree<E>> getPathFromNodeToRoot(Tree<E> node){
		if(this==node) {
			ArrayList path = new ArrayList<E>();
			path.add(this);
			return path;
		}
		
		for(Tree<E> child: children) {
			ArrayList<Tree<E>> path = child.getPathFromNodeToRoot(node);
			if (path!=null) {
				path.add(this);
				return path;
			}
		}
		
		return null;
	}
	
	/**
	 * Checks whether a particular tree node should be included when looking for certain nodes.
	 * (This is a function, wrapped in a class..)
	 * 
	 * @author Tim Molderez
	 */
	public interface Filter<E> {
		/**
		 * While traversing the tree.. does the current node match with the key?
		 * @return true if the current node matches with the key; false otherwise
		 */
		public boolean matches(E searchKey,E current);
	}
}
