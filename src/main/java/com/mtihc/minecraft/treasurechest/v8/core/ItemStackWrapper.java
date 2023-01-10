package com.mtihc.minecraft.treasurechest.v8.core;

import org.bukkit.Material;
import org.bukkit.configuration.serialization.ConfigurationSerializable;
import org.bukkit.inventory.ItemStack;

import java.util.LinkedHashMap;
import java.util.Map;
/**
 *  original deserialization code by Mitch,
 *  under GNU General Public License
 *  taken from <a href="https://github.com/HenrikoMagnifico/TreasureChestX/blob/main/src/main/java/com/mtihc/minecraft/treasurechest/v8/core/ItemStackWrapper.java">link</a>
 * This is just a wrapper class for ItemStack. In case we need to do some custom serialization.
 *
 * @author Mitch
 *
 */
public class ItemStackWrapper implements ConfigurationSerializable {

	private ItemStack stack;


	public ItemStack getItemStack() {
		return stack;
	}
	
	public void setItemStack(ItemStack stack) {
		if(stack == null) {
			this.stack = new ItemStack(Material.AIR);
		}
		else {
			this.stack = stack.clone();
		}
	}
	
	public ItemStackWrapper(ItemStack stack) {
		setItemStack(stack);
	}
	
	public ItemStackWrapper(Map<String, Object> values) {
		setItemStack( (ItemStack) values.get("stack"));
	}

	@Override
	public Map<String, Object> serialize() {
		Map<String, Object> values = new LinkedHashMap<String, Object>();
		values.put("stack", stack);
		
		return values;
	}
}
