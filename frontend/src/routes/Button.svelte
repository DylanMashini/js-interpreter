<script lang="ts">
  import { createEventDispatcher } from 'svelte';

  export let type: "button" | "submit" | "reset" = "button";
  export let disabled = false;
  export let className = "";

  const dispatch = createEventDispatcher();

  function handleClick(event: MouseEvent) {
    dispatch('click', event);
  }
</script>

<button
  {type}
  {disabled}
  on:click={handleClick}
  class={`px-4 py-2 rounded-md font-medium text-sm transition-colors
    focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500
    disabled:opacity-50 disabled:cursor-not-allowed
    bg-blue-600 text-white hover:bg-blue-700 ${className}`}
>
  <slot></slot>
</button>
