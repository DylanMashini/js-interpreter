<script lang="ts">
  import Button from './Button.svelte';
  import ScrollArea from './ScrollArea.svelte';
  import init, {run_js} from "$lib/interpreter";
  import {onMount} from "svelte";
  import { tick } from 'svelte';



  let interpreter_running = false;

  onMount(async () => {
        await init()
        interpreter_running = true;
    });

  let code = typeof window !== "undefined" && window.localStorage.getItem("interpreterCode") || '// Write your JavaScript code here\nconsole.log("Hello, World!");';
  let output = '';
  let error = typeof window !== "undefined" && window.localStorage.getItem("interpreterError") || "";

  async function runCode() {
    error = "";
    window.localStorage.removeItem("interpreterError");
    try {
    output = run_js(code);
    } catch (e) {
        error = window.interpreterError;
        window.localStorage.setItem("interpreterError", error);
        location.reload();
    }
  }

  $: typeof window !== "undefined" && window.localStorage.setItem("interpreterCode", code)
    async function handleKeyDown(event) {
    if (event.key === 'Tab') {
      event.preventDefault();

      const start = event.target.selectionStart;
      const end = event.target.selectionEnd;

      // Insert tab character at the cursor position
      event.target.value = code.substring(0, start) + '\t' + code.substring(end);
      code = code.substring(0, start) + '\t' + code.substring(end);

      // Update the cursor position
      event.target.selectionStart = event.target.selectionEnd = start + 1;
    }
  }

</script>

<div class="flex flex-col h-screen bg-gray-900 text-gray-100 p-4">
  <h1 class="text-2xl font-bold mb-4">JavaScript Playground</h1>
  <p>This is a interactive playground for the JS interpreter I wrote. When you execute code, it isn't run in the browsers runtime, insetad it runs in a runtime I wrote in Rust and compiled to WASM. The source code is available <a href="https://github.com/DylanMashini/js-interpreter" target="_blank" rel="noreferer noopener" class="text-blue-400 underline">on GitHub</a></p>
  <br>
  <div class="flex flex-col flex-grow">
    <div class="flex-grow mb-4">
      <textarea
        bind:value={code}
        on:keydown={handleKeyDown}
        class="w-full h-full p-4 bg-gray-800 text-gray-100 font-mono text-sm rounded-md resize-none focus:outline-none focus:ring-2 focus:ring-blue-500"
        spellcheck="false"
      ></textarea>
    </div>
    <Button on:click={runCode} className="mb-4">
      Run Code
    </Button>
    <div class="flex-grow">
      <h2 class="text-lg font-semibold mb-2">Output:</h2>
      <ScrollArea className="h-[200px] w-full rounded-md border border-gray-700 bg-gray-800 p-4">
      {#if error}
        <pre class="font-mono text-sm whitespace-pre-wrap text-red-500">{error}</pre>
      {:else}
        <pre class="font-mono text-sm whitespace-pre-wrap">{output}</pre>
      {/if}
      </ScrollArea>
    </div>
  </div>
</div>
