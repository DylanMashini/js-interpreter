import init, { run_js } from "./interpreter"

export type RunResult = {
    result: "success" | "error",
    output: String
}

declare global {
    interface Window {
        interpreterError: string | undefined;
    }
}


const run = async () => {
    await init();
    console.log("Worker Init")

    self.addEventListener("message", ({ data: code }) => {
        try {
            let output = run_js(code);
            let result: RunResult = {
                result: "success",
                output
            };
            self.postMessage(result)
        } catch {
            let result: RunResult = {
                result: "error",
                output: self.interpreterError || ""
            }
            self.postMessage(result);
            self.close();
        }

    })
}

run()
