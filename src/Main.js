
// module Main

exports.runProcessEff = function(cmd) {
  return function(args) {
    return function(errback) {
      return function(callback) {
        return function() {
          require('child_process').execFile(cmd, args, function(error, stdout, stderr) {
            if (error !== null) {
              errback(error)()
            } else {
              callback({ stdout: stdout, stderr: stderr })()
            }
          })
        }
      }
    }
  }
}

exports.yoloStringify = function(obj) { return JSON.stringify(obj) }
