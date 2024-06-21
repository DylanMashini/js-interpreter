<script lang="ts">
    import init, {run_js} from "$lib/interpreter"
    import {onMount} from "svelte"
    
    let code = "";
    let output = "";

    let ready = false;

    onMount(async () => {
        await init()
        ready = true;
    })

    const run_code = async () => {
        if (ready) {
            output = run_js(code);
            console.log(output)
        }
    }
</script>

<textarea class="w-full min-h-12" bind:value={code}></textarea>

<button on:click={run_code}>Run</button>

<div>
{@html output.replace("\n", "<br>")}
</div>