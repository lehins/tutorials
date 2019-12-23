# Setup

Step-by-step guide of setting up IHaskell on a Windows machine with Ubuntu 18.04 LTS in
WSL.


Enable WSL:

https://docs.microsoft.com/en-us/windows/wsl/install-on-server


* In powershell
```
Enable-WindowsOptionalFeature -Online -FeatureName Microsoft-Windows-Subsystem-Linux
```

* Reboot


* Download Ubuntu 18.04 for here: https://docs.microsoft.com/en-us/windows/wsl/install-manual


* Extract and run `ubuntu1804`


* Setup username and password

## Anaconda (Python)

QuickStart: https://www.digitalocean.com/community/tutorials/how-to-install-anaconda-on-ubuntu-18-04-quickstart


* Download Anaconda for Linux from
  https://www.anaconda.com/distribution/#download-section (command below does that):

```
$ curl -O https://repo.anaconda.com/archive/Anaconda3-2019.10-Linux-x86_64.sh
$ bash Anaconda3-2019.10-Linux-x86_64.sh
```

* press enter,

* say "YES", enter

* Select home, use default: /home/user/anaconda3

* grab a cup of coffee

* Answer "yes" at the end for initialization of anaconda

run:
```
$ eval "$(/home/lehins/anaconda3/bin/conda shell.bashhook)"
$ conda init bash
```

From then on `conda activate/deactivate`

## Getting Stack

```
$ sudo apt-get update
$ curl -sSL https://get.haskellstack.org/ | sh
```

It will ask for user password.


## Getting your repo:


```
$ git clone https://github.com/lehins/color-space.git
$ cd color-space
$ stack test
```

Get a nice meal...


## Setup IHaskell

Get nodejs installed, if it is not already available
```
conda install nodejs
```

Now we get the haskell extension working.

* Install dependencies. At some point it will ask if it should skip asking more questions,
  make sure to say "yes", it'll go quicker.

```
sudo apt-get install -y libtinfo-dev libzmq3-dev libcairo2-dev libpango1.0-dev libmagic-dev libblas-dev liblapack-dev
```

Install the actual ihaskell and extension for jupyter
```
stack install ihaskell
ihaskell install --stack
stack exec -- jupyter labextension install jupyterlab-ihaskell
```


```
stack exec -- jupyter lab
```

Ctrl+Shift+C the full URL
