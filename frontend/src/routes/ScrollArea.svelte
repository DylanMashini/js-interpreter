<script lang="ts">
  export let className = "";
</script>

<div class={`overflow-auto ${className}`}>
  <slot></slot>
</div>

<style lang="postcss">
  div {
    scrollbar-width: thin;
    scrollbar-color: theme('colors.gray.600') theme('colors.gray.800');
  }

  div::-webkit-scrollbar {
    width: 8px;
  }

  div::-webkit-scrollbar-track {
    background: theme('colors.gray.800');
  }

  div::-webkit-scrollbar-thumb {
    background-color: theme('colors.gray.600');
    border-radius: 4px;
    border: 2px solid theme('colors.gray.800');
  }
</style>
