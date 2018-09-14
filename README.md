# CurveClassification

Minimal web application to train and evaluate random forests for classification of omics-type experiments.

## Usage

A detailed description can be found in the provided [PDF](https://github.com/kusterlab/curveClassification_shiny/raw/master/manual/curveClassification_manual.pdf), but we also provide a video tutorial on [Youtube](https://www.youtube.com/watch?v=TzBBvuAYtkg&list=PLCBcz0G7FF6-Ts1ljvqwEO0uvRXvu2O63)

## Building your own CurveClassification instance

### Requirenment
- [Docker](https://www.docker.com/)
  - [How to install for linux](https://docs.docker.com/install/linux/docker-ce/ubuntu/)
  - [How to install for Windows](https://www.docker.com/get-started). 
  - [How to install for MacOS](https://www.docker.com/get-started)

  since June 20 you have to register to be able to download docker for Windows and MacOS.
  

### Execution
*Hint*: The first installation of the programm takes up to 15 minutes.
#### Linux

You need `sudo` rights if your user is not part of the group `docker` 
```
git clone https://github.com/kusterlab/curveClassification_shiny.git
make run
```

#### Windows
1. Open `PowerShell`
```
docker build https://github.com/kusterlab/curveClassification_shiny
make run
```

#### MacOS
You can either follow the installation for Linux


The server/computer should now run a *CurveClassification* instance, which is available on port `8787`.

