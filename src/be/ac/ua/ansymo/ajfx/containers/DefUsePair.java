package be.ac.ua.ansymo.ajfx.containers;

import java.util.HashMap;
import java.util.Map;

import abc.weaving.matching.ShadowMatch;

public class DefUsePair {
	public TransferFunction defs;
	public Map<ShadowMatch, TransferFunction> uses;
}
