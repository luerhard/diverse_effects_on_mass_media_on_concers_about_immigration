from pathlib import Path
import subprocess

modelpath = Path("models/stmobjects/")
heldoutpath = Path("models/heldoutobjects/")

topicnums = [30,40,50,60,70,80,90,100,110,120,150,180]
second_run = [65,75,72,68,71,69,63,77]
third_run = [73,74,76,78,67,66]
topicnums += second_run
topicnums += third_run

for num in topicnums:
   
    done = []
    for file in modelpath.iterdir():
        _,_,n = str(file.with_suffix("")).split("_")
        done.append(n)
    
    heldout_done = []
    for file in heldoutpath.iterdir():
        _,_,n = str(file.with_suffix("")).split("_")
        heldout_done.append(n)
        
    if not str(num) in done:
        print(f"topicnum: {num}")
        proc = subprocess.Popen(["Rscript", "02_run_model.R", f"--ntopics={num}"], 
                                stdout=subprocess.PIPE, 
                                stderr=subprocess.PIPE, 
                                universal_newlines=True)
        for line in iter(proc.stdout.readline, ""):
            print(line.rstrip())

    if not str(num) in heldout_done:
        print(f"Heldout topicnum: {num}")
        proc = subprocess.Popen(["Rscript", "03_run_heldout.R", f"--ntopics={num}"], 
                                stdout=subprocess.PIPE, 
                                stderr=subprocess.PIPE, 
                                universal_newlines=True)
        for line in iter(proc.stdout.readline, ""):
            print(line.rstrip())
