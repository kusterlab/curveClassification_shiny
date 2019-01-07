IMAGE = curve_classification
PORT_IN = 8787
PORT_OUT = 8787
MEMORY_LIMIT = 4096mb
CPU_LIMIT = 1

build:
		docker build -t $(IMAGE) .

build_fresh:
		docker build --no-cache -t $(IMAGE) .

run: build
		docker run -d --name $(IMAGE) \
				--memory=$(MEMORY_LIMIT) \
				--cpus=$(CPU_LIMIT) \
				--restart unless-stopped \
				-p $(PORT_IN):$(PORT_IN) $(IMAGE)
jump: build
		docker run -it \
				--memory=$(MEMORY_LIMIT) \
				--cpus=$(CPU_LIMIT) \
				-p $(PORT_IN):$(PORT_IN) $(IMAGE) bash


clean:
		docker kill $(IMAGE)
			docker rm $(IMAGE)

exec:
		docker exec --user root -it $(IMAGE) bash

