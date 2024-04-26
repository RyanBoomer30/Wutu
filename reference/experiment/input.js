// Enter any texts ( User input) 
process.stdin.on('data', line => { 
    console.log(`You typed ${line.toString()}`); 
    process.exit(); 
  });