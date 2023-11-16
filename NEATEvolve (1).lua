-- MarI/O by SethBling
-- Feel free to use this code, but please do not redistribute it.
-- Intended for use with the BizHawk emulator and Super Mario World or Super Mario Bros. ROM.
-- For SMW, make sure you have a save state named "DP1.state" at the beginning of a level,
-- and put a copy in both the Lua folder and the root directory of BizHawk.

-- Verifica el nombre del juego actual y establece variables específicas según el juego.
if gameinfo.getromname() == "Super Mario World (USA)" then
	Filename = "DP1.state"
	ButtonNames = {
		"A",
		"B",
		"X",
		"Y",
		"Up",
		"Down",
		"Left",
		"Right",
	}
elseif gameinfo.getromname() == "Super Mario Bros." then
	Filename = "SMB1-1.state"
	ButtonNames = {
		"A",
		"B",
		"Up",
		"Down",
		"Left",
		"Right",
	}
end

-- Define variables relacionadas con la estructura de la red neuronal y el algoritmo de aprendizaje.
--Configuración de la caja
--Radio de la caja
BoxRadius = 6 
--Tamaño de la entrada
InputSize = (BoxRadius*2+1)*(BoxRadius*2+1)

--Configuración de la red neuronal
--Número de entradas
Inputs = InputSize + 1
--Número de salidas (especificado en otro lugar)
Outputs = '#ButtonNames'  

--Tamaño de la población
Population = 300 
--Diferencia de genes disjuntos
DeltaDisjoint = 2.0
--Diferencia de pesos
DeltaWeights = 0.4 
--Umbral de diferencia
DeltaThreshold = 1.0 

--Número de especies inactivas
StaleSpecies = 15  

--Probabilidad de mutación de conexiones
MutateConnectionsChance = 0.25  
--Probabilidad de perturbación
PerturbChance = 0.90 
--Probabilidad de cruce 
CrossoverChance = 0.75  
--Probabilidad de mutación de enlaces
LinkMutationChance = 2.0 
--Probabilidad de mutación de nodo 
NodeMutationChance = 0.50 
--Probabilidad de mutación de sesgo 
BiasMutationChance = 0.40
--Tamaño del paso
StepSize = 0.1
--Probabilidad de deshabilitar mutación
DisableMutationChance = 0.4
--Probabilidad de habilitar mutación
EnableMutationChance = 0.2  

--Constante de tiempo de espera
TimeoutConstant = 20  
--Número máximo de nodos permitidos
MaxNodes = 1000000 

--[[ 
Descripción:
  Esta función obtiene la posición actual del personaje principal en el juego, así como la posición de la pantalla,
  dependiendo del juego en el que se esté jugando (ya sea "Super Mario World (USA)" o "Super Mario Bros.").

Parámetros:
  Ninguno.

Retorno:
  Ninguno. Esta función actualiza las siguientes variables globales:
  - marioX (número): La posición X actual de Mario.
  - marioY (número): La posición Y actual de Mario.
  - screenX (número): La posición X de la pantalla en relación con Mario.
  - screenY (número): La posición Y de la pantalla en relación con Mario.
--]]
function getPositions()
	if gameinfo.getromname() == "Super Mario World (USA)" then
	  marioX = memory.read_s16_le(0x94)
	  marioY = memory.read_s16_le(0x96)
	  
	  local layer1x = memory.read_s16_le(0x1A)
	  local layer1y = memory.read_s16_le(0x1C)
	  
	  screenX = marioX - layer1x
	  screenY = marioY - layer1y
	elseif gameinfo.getromname() == "Super Mario Bros." then
	  marioX = memory.readbyte(0x6D) * 0x100 + memory.readbyte(0x86)
	  marioY = memory.readbyte(0x03B8) + 16
  
	  screenX = memory.readbyte(0x03AD)
	  screenY = memory.readbyte(0x03B8)
	end
end
  

--[[ 
Descripción:
  Obtiene el tipo de bloque o tile en la posición relativa al personaje principal (Mario) en el juego actual,
  ya sea "Super Mario World (USA)" o "Super Mario Bros." Retorna:
    - 0: Espacio vacío o bloque vacío.
    - 1: Bloque o tile con contenido (sólido).

Parámetros:
  - dx (número): Desplazamiento horizontal desde la posición de Mario.
  - dy (número): Desplazamiento vertical desde la posición de Mario.

Retorno:
  - Tipo de bloque o tile (número):
    - 0: Espacio vacío o bloque vacío.
    - 1: Bloque o tile con contenido (sólido).
--]]
function getTile(dx, dy)
	if gameinfo.getromname() == "Super Mario World (USA)" then
		x = math.floor((marioX+dx+8)/16)
		y = math.floor((marioY+dy)/16)
		
		return memory.readbyte(0x1C800 + math.floor(x/0x10)*0x1B0 + y*0x10 + x%0x10)
	elseif gameinfo.getromname() == "Super Mario Bros." then
		local x = marioX + dx + 8
		local y = marioY + dy - 16
		local page = math.floor(x/256)%2

		local subx = math.floor((x%256)/16)
		local suby = math.floor((y - 32)/16)
		local addr = 0x500 + page*13*16+suby*16+subx
		
		if suby >= 13 or suby < 0 then
			return 0
		end
		
		if memory.readbyte(addr) ~= 0 then
			return 1
		else
			return 0
		end
	end
end

--[[ 
Descripción:
  Esta función obtiene la información de las entidades (sprites) presentes en el juego, dependiendo del juego en el que se esté jugando
  (ya sea "Super Mario World (USA)" o "Super Mario Bros."). La información incluye las coordenadas (x, y) de las entidades activas.

Parámetros:
  Ninguno.

Retorno:
  - sprites (tabla): Una tabla que contiene información sobre las entidades activas. Cada entrada en la tabla es una tabla que
    contiene las coordenadas (x, y) de una entidad. Las coordenadas se almacenan como pares clave-valor en las subtablas.
--]]
function getSprites()
	if gameinfo.getromname() == "Super Mario World (USA)" then
		local sprites = {}
		for slot=0,11 do
			local status = memory.readbyte(0x14C8+slot)
			if status ~= 0 then
				spritex = memory.readbyte(0xE4+slot) + memory.readbyte(0x14E0+slot)*256
				spritey = memory.readbyte(0xD8+slot) + memory.readbyte(0x14D4+slot)*256
				sprites[#sprites+1] = {["x"]=spritex, ["y"]=spritey}
			end
		end		
		
		return sprites
	elseif gameinfo.getromname() == "Super Mario Bros." then
		local sprites = {}
		for slot=0,4 do
			local enemy = memory.readbyte(0xF+slot)
			if enemy ~= 0 then
				local ex = memory.readbyte(0x6E + slot)*0x100 + memory.readbyte(0x87+slot)
				local ey = memory.readbyte(0xCF + slot)+24
				sprites[#sprites+1] = {["x"]=ex,["y"]=ey}
			end
		end
		
		return sprites
	end
end

--[[ 
Descripción:
  Esta función obtiene la información de las entidades extendidas presentes en el juego "Super Mario World (USA)".
  La información incluye las coordenadas (x, y) de las entidades extendidas activas.

Parámetros:
  Ninguno.

Retorno:
  - extended (tabla): Una tabla que contiene información sobre las entidades extendidas activas.
    Cada entrada en la tabla es una tabla que contiene las coordenadas (x, y) de una entidad extendida.
    Las coordenadas se almacenan como pares clave-valor en las subtablas.
--]]
function getExtendedSprites()
	if gameinfo.getromname() == "Super Mario World (USA)" then
		local extended = {}
		for slot=0,11 do
			local number = memory.readbyte(0x170B+slot)
			if number ~= 0 then
				spritex = memory.readbyte(0x171F+slot) + memory.readbyte(0x1733+slot)*256
				spritey = memory.readbyte(0x1715+slot) + memory.readbyte(0x1729+slot)*256
				extended[#extended+1] = {["x"]=spritex, ["y"]=spritey}
			end
		end		
		
		return extended
	elseif gameinfo.getromname() == "Super Mario Bros." then
		return {}
	end
end

--[[ 
Descripción:
  Esta función obtiene información sobre el entorno del juego y la posición de Mario y otras entidades, luego genera una matriz
  de entradas (inputs) que representan el entorno y las situaciones del juego. Estas entradas se utilizan en el contexto de
  aprendizaje automático para entrenar un agente de IA.

Parámetros:
  Ninguno.

Retorno:
  - inputs (tabla): Una tabla que representa las entradas del modelo de aprendizaje automático.
    Cada elemento de la tabla corresponde a una casilla de un cuadrado (box) alrededor de Mario, y contiene un valor que
    representa el entorno de esa casilla:
    - 0: Casilla vacía.
    - 1: Casilla sólida o bloque.
    - -1: Presencia de un sprite cercano (entidad enemiga o personaje).
--]]
function getInputs()
	getPositions()
	
	sprites = getSprites()
	extended = getExtendedSprites()
	
	local inputs = {}
	
	for dy=-BoxRadius*16,BoxRadius*16,16 do
		for dx=-BoxRadius*16,BoxRadius*16,16 do
			inputs[#inputs+1] = 0
			
			tile = getTile(dx, dy)
			if tile == 1 and marioY+dy < 0x1B0 then
				inputs[#inputs] = 1
			end
			
			for i = 1,#sprites do
				distx = math.abs(sprites[i]["x"] - (marioX+dx))
				disty = math.abs(sprites[i]["y"] - (marioY+dy))
				if distx <= 8 and disty <= 8 then
					inputs[#inputs] = -1
				end
			end

			for i = 1,#extended do
				distx = math.abs(extended[i]["x"] - (marioX+dx))
				disty = math.abs(extended[i]["y"] - (marioY+dy))
				if distx < 8 and disty < 8 then
					inputs[#inputs] = -1
				end
			end
		end
	end
	
	--mariovx = memory.read_s8(0x7B)
	--mariovy = memory.read_s8(0x7D)
	
	return inputs
end

--[[ 
Descripción:
  Esta función calcula el valor de la función sigmoide de un número real dado. La función sigmoide se utiliza comúnmente en
  aprendizaje automático y redes neuronales para introducir no linealidad en los modelos.

Parámetros:
  - x (número): El número real para el cual se calculará la función sigmoide.

Retorno:
  - Valor de la función sigmoide (número): El resultado de aplicar la función sigmoide al valor de entrada 'x'.
    El valor de retorno está en el rango [-1, 1].
--]]
function sigmoid(x)
	return 2 / (1 + math.exp(-4.9 * x)) - 1
  end
  

--[[ 
Descripción:
  Esta función se utiliza en el contexto de algoritmos genéticos y neuroevolución para generar una innovación única.
  Cada vez que se llama a esta función, incrementa el valor de innovación en el grupo (pool) en 1 y devuelve el nuevo valor de innovación.

Parámetros:
  Ninguno.

Retorno:
  - Nueva innovación (número): El nuevo valor de innovación generado. Este valor se utiliza para rastrear innovaciones
    únicas en el contexto de algoritmos de evolución.
--]]
function newInnovation()
	pool.innovation = pool.innovation + 1  -- Incrementa el valor de innovación en el grupo (pool) en 1.
	return pool.innovation  -- Devuelve el nuevo valor de innovación.
  end
  
--[[ 
Descripción:
  Esta función se utiliza para crear un nuevo grupo (pool) en el contexto de algoritmos genéticos y neuroevolución. Un grupo
  (pool) representa un conjunto de especies y genomas que evolucionan juntos. Cada grupo tiene atributos como la generación,
  la innovación, el índice de la especie y el genoma actual, el cuadro (frame) actual y la máxima aptitud registrada.

Parámetros:
  Ninguno.

Retorno:
  - pool (tabla): Una nueva instancia de grupo (pool) con atributos iniciales.
    - species (tabla): Una tabla que almacena las especies dentro del grupo.
    - generation (número): El número de generación del grupo.
    - innovation (número): El valor de innovación inicial del grupo.
    - currentSpecies (número): El índice de la especie actual dentro del grupo.
    - currentGenome (número): El índice del genoma actual dentro de la especie actual.
    - currentFrame (número): El cuadro (frame) actual.
    - maxFitness (número): La máxima aptitud registrada en el grupo.
--]]
function newPool()
	local pool = {}
	pool.species = {}
	pool.generation = 0
	pool.innovation = Outputs
	pool.currentSpecies = 1
	pool.currentGenome = 1
	pool.currentFrame = 0
	pool.maxFitness = 0
	
	return pool
end

--[[ 
Descripción:
  Esta función se utiliza para crear una nueva especie en el contexto de algoritmos genéticos y neuroevolución.
  Cada especie contiene información sobre su mejor aptitud, su "edad" (staleness), una lista de genomas que pertenecen a la especie
  y un valor promedio de aptitud de la especie.

Parámetros:
  Ninguno.

Retorno:
  - species (tabla): Una nueva instancia de especie con atributos iniciales.
    - topFitness (número): La mejor aptitud registrada en la especie.
    - staleness (número): La "edad" o tiempo sin mejora en la aptitud de la especie.
    - genomes (tabla): Una tabla que almacena los genomas que pertenecen a la especie.
    - averageFitness (número): El valor promedio de aptitud de la especie.
--]]
function newSpecies()
	local species = {}
	species.topFitness = 0
	species.staleness = 0
	species.genomes = {}
	species.averageFitness = 0
	
	return species
end

--[[ 
Descripción:
  Esta función se utiliza para crear un nuevo genoma en el contexto de algoritmos genéticos y neuroevolución.
  Cada genoma contiene información sobre sus genes, aptitud, aptitud ajustada, red neuronal, número máximo de neuronas,
  rango global, y tasas de mutación para diversas operaciones genéticas.

Parámetros:
  Ninguno.

Retorno:
  - genome (tabla): Una nueva instancia de genoma con atributos iniciales.
    - genes (tabla): Una tabla que almacena los genes del genoma.
    - fitness (número): La aptitud del genoma.
    - adjustedFitness (número): La aptitud ajustada del genoma.
    - network (tabla): La representación de la red neuronal del genoma.
    - maxneuron (número): El número máximo de neuronas en la red neuronal.
    - globalRank (número): El rango global del genoma.
    - mutationRates (tabla): Una tabla que almacena tasas de mutación para operaciones genéticas.
    - "connections" (número): Tasa de mutación para conexiones genéticas.
    - "link" (número): Tasa de mutación para enlaces genéticos.
    - "bias" (número): Tasa de mutación para sesgos genéticos.
    - "node" (número): Tasa de mutación para mutaciones de nodos.
    - "enable" (número): Tasa de mutación para habilitar conexiones.
    - "disable" (número): Tasa de mutación para deshabilitar conexiones.
    - "step" (número): Tasa de mutación para cambios en el tamaño de paso.
--]]
function newGenome()
	local genome = {}
	genome.genes = {}
	genome.fitness = 0
	genome.adjustedFitness = 0
	genome.network = {}
	genome.maxneuron = 0
	genome.globalRank = 0
	genome.mutationRates = {}
	genome.mutationRates["connections"] = MutateConnectionsChance
	genome.mutationRates["link"] = LinkMutationChance
	genome.mutationRates["bias"] = BiasMutationChance
	genome.mutationRates["node"] = NodeMutationChance
	genome.mutationRates["enable"] = EnableMutationChance
	genome.mutationRates["disable"] = DisableMutationChance
	genome.mutationRates["step"] = StepSize
	
	return genome
end

--[[ 
Descripción:
  Esta función se utiliza para crear una copia de un genoma existente. La copia incluye todos los genes, tasas de mutación y
  otros atributos del genoma original. Se utiliza comúnmente en algoritmos genéticos y neuroevolución para generar variaciones
  de genomas existentes.

Parámetros:
  - genome (tabla): El genoma que se desea copiar.

Retorno:
  - genome2 (tabla): Una copia del genoma original con todos sus atributos y genes duplicados.
--]]
function copyGenome(genome)
	local genome2 = newGenome()
	for g=1,#genome.genes do
		table.insert(genome2.genes, copyGene(genome.genes[g]))
	end
	genome2.maxneuron = genome.maxneuron
	genome2.mutationRates["connections"] = genome.mutationRates["connections"]
	genome2.mutationRates["link"] = genome.mutationRates["link"]
	genome2.mutationRates["bias"] = genome.mutationRates["bias"]
	genome2.mutationRates["node"] = genome.mutationRates["node"]
	genome2.mutationRates["enable"] = genome.mutationRates["enable"]
	genome2.mutationRates["disable"] = genome.mutationRates["disable"]
	
	return genome2
end

--[[ 
Descripción:
  Esta función se utiliza para crear un genoma básico en el contexto de algoritmos genéticos y neuroevolución. El genoma
  básico es una instancia de genoma con atributos iniciales y puede utilizarse como punto de partida para la evolución.

Parámetros:
  Ninguno.

Retorno:
  - genome (tabla): Una nueva instancia de genoma con atributos iniciales.
    - maxneuron (número): El número máximo de neuronas en el genoma, basado en el número de entradas (Inputs).
    - genes (tabla): Una tabla que almacena los genes del genoma.
    - mutationRates (tabla): Una tabla que almacena tasas de mutación para diversas operaciones genéticas.
    - fitness (número): La aptitud del genoma (inicialmente en 0).
--]]
function basicGenome()
	local genome = newGenome()
	local innovation = 1

	genome.maxneuron = Inputs
	mutate(genome)
	
	return genome
end

--[[ 
Descripción:
  Esta función se utiliza para crear un nuevo gen de conexión (gene) en el contexto de algoritmos genéticos y neuroevolución.
  Cada gen de conexión representa una conexión entre dos neuronas en una red neuronal y almacena información sobre las neuronas
  de entrada y salida, el peso de la conexión, si está habilitada o deshabilitada, y su valor de innovación.

Parámetros:
  Ninguno.

Retorno:
  	- gene (tabla): Una nueva instancia de gen de conexión con atributos iniciales.
    - into (número): El índice de la neurona de entrada en la conexión.
    - out (número): El índice de la neurona de salida en la conexión.
    - weight (número): El peso de la conexión.
    - enabled (booleano): Indica si la conexión está habilitada (true) o deshabilitada (false).
    - innovation (número): El valor de innovación de la conexión.
--]]
function newGene()
	local gene = {}
	gene.into = 0
	gene.out = 0
	gene.weight = 0.0
	gene.enabled = true
	gene.innovation = 0
	
	return gene
end

--[[ 
Descripción:
  Esta función se utiliza para crear una copia de un gen de conexión (gene) existente. La copia incluye todos los atributos
  del gen original, como las neuronas de entrada y salida, el peso de la conexión, su estado de habilitación y el valor de
  innovación. Se utiliza comúnmente en algoritmos genéticos y neuroevolución para generar variaciones de genes existentes.

Parámetros:
  - gene (tabla): El gen de conexión que se desea copiar.

Retorno:
  - gene2 (tabla): Una copia del gen de conexión original con todos sus atributos duplicados.
--]]
function copyGene(gene)
	local gene2 = newGene()
	gene2.into = gene.into
	gene2.out = gene.out
	gene2.weight = gene.weight
	gene2.enabled = gene.enabled
	gene2.innovation = gene.innovation
	
	return gene2
end

--[[ 
Descripción:
  Esta función se utiliza para crear un nuevo neurona en el contexto de algoritmos genéticos y neuroevolución.
  Cada neurona contiene una lista de conexiones entrantes (genes de conexión) y un valor de activación inicial.

Parámetros:
  Ninguno.

Retorno:
  - neuron (tabla): Una nueva instancia de neurona con atributos iniciales.
    - incoming (tabla): Una tabla que almacena las conexiones entrantes (genes de conexión) a la neurona.
    - value (número): El valor de activación inicial de la neurona.
--]]
function newNeuron()
	local neuron = {}
	neuron.incoming = {}
	neuron.value = 0.0
	
	return neuron
end

--[[ 
Descripción:
  Esta función se utiliza para generar una red neuronal a partir de un genoma dado en el contexto de algoritmos genéticos y
  neuroevolución. La red neuronal incluye neuronas de entrada, neuronas de salida y neuronas ocultas, así como las conexiones
  entre ellas, basadas en los genes del genoma.

Parámetros:
  - genome (tabla): El genoma a partir del cual se generará la red neuronal.

Retorno:
  Ninguno.
--]]
function generateNetwork(genome)
	local network = {}
	network.neurons = {}
	
	for i=1,Inputs do
		network.neurons[i] = newNeuron()
	end
	
	for o=1,Outputs do
		network.neurons[MaxNodes+o] = newNeuron()
	end
	
	table.sort(genome.genes, function (a,b)
		return (a.out < b.out)
	end)
	for i=1,#genome.genes do
		local gene = genome.genes[i]
		if gene.enabled then
			if network.neurons[gene.out] == nil then
				network.neurons[gene.out] = newNeuron()
			end
			local neuron = network.neurons[gene.out]
			table.insert(neuron.incoming, gene)
			if network.neurons[gene.into] == nil then
				network.neurons[gene.into] = newNeuron()
			end
		end
	end
	
	genome.network = network
end

--[[ 
Descripción:
  Esta función se utiliza para evaluar una red neuronal dada con una serie de entradas, lo que produce salidas calculadas
  basadas en las conexiones y pesos de la red. La función aplica las entradas a la red y calcula los valores de activación
  en las neuronas, lo que resulta en una serie de salidas que representan decisiones o acciones.

Parámetros:
  - network (tabla): La red neuronal que se va a evaluar.
  - inputs (tabla): Un arreglo de valores que representan las entradas para la red neuronal.

Retorno:
  - outputs (tabla): Un arreglo de salidas calculadas por la red neuronal.
--]]
function evaluateNetwork(network, inputs)
	table.insert(inputs, 1)
	if #inputs ~= Inputs then
		console.writeline("Incorrect number of neural network inputs.")
		return {}
	end
	
	for i=1,Inputs do
		network.neurons[i].value = inputs[i]
	end
	
	for _,neuron in pairs(network.neurons) do
		local sum = 0
		for j = 1,#neuron.incoming do
			local incoming = neuron.incoming[j]
			local other = network.neurons[incoming.into]
			sum = sum + incoming.weight * other.value
		end
		
		if #neuron.incoming > 0 then
			neuron.value = sigmoid(sum)
		end
	end
	
	local outputs = {}
	for o=1,Outputs do
		local button = "P1 " .. ButtonNames[o]
		if network.neurons[MaxNodes+o].value > 0 then
			outputs[button] = true
		else
			outputs[button] = false
		end
	end
	
	return outputs
end

--[[ 
Descripción:
  Esta función se utiliza para realizar la cruza (crossover) entre dos genomas g1 y g2 en el contexto de algoritmos genéticos
  y neuroevolución. La cruza combina los genes de ambos genomas para crear un nuevo genoma hijo. Los genes se eligen
  aleatoriamente de ambos genomas, y el genoma con mayor aptitud (fitness) se coloca como g1 para asegurarse de que el hijo
  herede las características del genoma más apto.

Parámetros:
  - g1 (tabla): El primer genoma a cruzar.
  - g2 (tabla): El segundo genoma a cruzar.

Retorno:
  - child (tabla): El genoma hijo resultante de la cruza.
--]]
function crossover(g1, g2)
	-- Make sure g1 is the higher fitness genome
	if g2.fitness > g1.fitness then
		tempg = g1
		g1 = g2
		g2 = tempg
	end

	local child = newGenome()
	
	local innovations2 = {}
	for i=1,#g2.genes do
		local gene = g2.genes[i]
		innovations2[gene.innovation] = gene
	end
	
	for i=1,#g1.genes do
		local gene1 = g1.genes[i]
		local gene2 = innovations2[gene1.innovation]
		if gene2 ~= nil and math.random(2) == 1 and gene2.enabled then
			table.insert(child.genes, copyGene(gene2))
		else
			table.insert(child.genes, copyGene(gene1))
		end
	end
	
	child.maxneuron = math.max(g1.maxneuron,g2.maxneuron)
	
	for mutation,rate in pairs(g1.mutationRates) do
		child.mutationRates[mutation] = rate
	end
	
	return child
end

--[[ 
Descripción:
  Esta función se utiliza para seleccionar aleatoriamente una neurona de un conjunto de genes dados. Puede seleccionar
  cualquier neurona que no sea de entrada, o puede incluir neuronas de entrada si se especifica el parámetro nonInput.
  La selección aleatoria se basa en la cantidad total de neuronas disponibles en los genes.

Parámetros:
  - genes (tabla): Un arreglo de genes que contienen información sobre las conexiones entre neuronas.
  - nonInput (booleano, opcional): Un indicador que controla si se deben incluir neuronas de entrada en la selección aleatoria.
    Si es verdadero, se excluyen las neuronas de entrada; si es falso o no se proporciona, se incluyen las neuronas de entrada.

Retorno:
  - n (número): El índice de la neurona seleccionada aleatoriamente.
--]]
function randomNeuron(genes, nonInput)
	local neurons = {}
	if not nonInput then
		for i=1,Inputs do
			neurons[i] = true
		end
	end
	for o=1,Outputs do
		neurons[MaxNodes+o] = true
	end
	for i=1,#genes do
		if (not nonInput) or genes[i].into > Inputs then
			neurons[genes[i].into] = true
		end
		if (not nonInput) or genes[i].out > Inputs then
			neurons[genes[i].out] = true
		end
	end

	local count = 0
	for _,_ in pairs(neurons) do
		count = count + 1
	end
	local n = math.random(1, count)
	
	for k,v in pairs(neurons) do
		n = n-1
		if n == 0 then
			return k
		end
	end
	
	return 0
end

--[[ 
Descripción:
  Esta función se utiliza para verificar si un arreglo de genes contiene una conexión específica representada por un gen de conexión
  proporcionado. Compara el gen de conexión proporcionado (link) con cada gen de conexión en el arreglo de genes para determinar si
  ya existe una conexión idéntica en el arreglo.

Parámetros:
  - genes (tabla): Un arreglo de genes que contiene información sobre las conexiones entre neuronas.
  - link (tabla): El gen de conexión que se desea verificar si está contenido en el arreglo de genes.

Retorno:
  - true (booleano): Devuelve verdadero si el gen de conexión (link) está contenido en el arreglo de genes.
  - nil (nulo): Devuelve nulo si el gen de conexión (link) no está contenido en el arreglo de genes.
--]]
function containsLink(genes, link)
	for i=1,#genes do
		local gene = genes[i]
		if gene.into == link.into and gene.out == link.out then
			return true
		end
	end
end

--[[ 
Descripción:
  Esta función se utiliza para aplicar una mutación puntual a los genes de un genoma en el contexto de algoritmos genéticos y
  neuroevolución. La mutación puntual implica ajustar el peso de las conexiones de manera aleatoria para introducir variabilidad
  genética en la población de genomas.

Parámetros:
  - genome (tabla): El genoma al que se le aplicará la mutación puntual.

Retorno:
  Ninguno.
--]]
function pointMutate(genome)
	local step = genome.mutationRates["step"]
	
	for i=1,#genome.genes do
		local gene = genome.genes[i]
		if math.random() < PerturbChance then
			gene.weight = gene.weight + math.random() * step*2 - step
		else
			gene.weight = math.random()*4-2
		end
	end
end

--[[ 
Función: linkMutate

Descripción:
  Esta función se utiliza para aplicar una mutación de enlace (link mutation) a un genoma en el contexto de algoritmos genéticos y
  neuroevolución. La mutación de enlace implica agregar una nueva conexión entre dos neuronas en el genoma. La función selecciona
  aleatoriamente dos neuronas, una como entrada y otra como salida, y crea una nueva conexión entre ellas.

Parámetros:
  - genome (tabla): El genoma al que se le aplicará la mutación de enlace.
  - forceBias (booleano, opcional): Un indicador que controla si se debe forzar una de las neuronas a ser la de sesgo (bias).
    Si es verdadero, la neurona de entrada se fuerza como la de sesgo; si es falso o no se proporciona, no se fuerza una neurona de
    sesgo.

Retorno:
  Ninguno.
--]]
function linkMutate(genome, forceBias)
	local neuron1 = randomNeuron(genome.genes, false)
	local neuron2 = randomNeuron(genome.genes, true)
	 
	local newLink = newGene()
	if neuron1 <= Inputs and neuron2 <= Inputs then
		--Both input nodes
		return
	end
	if neuron2 <= Inputs then
		-- Swap output and input
		local temp = neuron1
		neuron1 = neuron2
		neuron2 = temp
	end

	newLink.into = neuron1
	newLink.out = neuron2
	if forceBias then
		newLink.into = Inputs
	end
	
	if containsLink(genome.genes, newLink) then
		return
	end
	newLink.innovation = newInnovation()
	newLink.weight = math.random()*4-2
	
	table.insert(genome.genes, newLink)
end

--[[ 
Descripción:
  Esta función se utiliza para aplicar una mutación de nodo (node mutation) a un genoma en el contexto de algoritmos genéticos y
  neuroevolución. La mutación de nodo implica dividir una conexión existente en dos conexiones más pequeñas y agregar una nueva neurona
  intermedia. Esto permite a la red neuronal crear una nueva neurona y reorganizar sus conexiones.

Parámetros:
  - genome (tabla): El genoma al que se le aplicará la mutación de nodo.

Retorno:
  Ninguno.
--]]
function nodeMutate(genome)
	if #genome.genes == 0 then
		return
	end

	genome.maxneuron = genome.maxneuron + 1

	local gene = genome.genes[math.random(1,#genome.genes)]
	if not gene.enabled then
		return
	end
	gene.enabled = false
	
	local gene1 = copyGene(gene)
	gene1.out = genome.maxneuron
	gene1.weight = 1.0
	gene1.innovation = newInnovation()
	gene1.enabled = true
	table.insert(genome.genes, gene1)
	
	local gene2 = copyGene(gene)
	gene2.into = genome.maxneuron
	gene2.innovation = newInnovation()
	gene2.enabled = true
	table.insert(genome.genes, gene2)
end

--[[ 
Descripción:
  Esta función se utiliza para aplicar una mutación de habilitar o deshabilitar una conexión en un genoma en el contexto de algoritmos
  genéticos y neuroevolución. La mutación permite habilitar o deshabilitar de manera aleatoria una conexión existente en el genoma.

Parámetros:
  - genome (tabla): El genoma al que se le aplicará la mutación de habilitar o deshabilitar una conexión.
  - enable (booleano): Un indicador que controla si se debe habilitar (verdadero) o deshabilitar (falso) una conexión existente en el genoma.

Retorno:
  Ninguno.
--]]
function enableDisableMutate(genome, enable)
	local candidates = {}
	for _,gene in pairs(genome.genes) do
		if gene.enabled == not enable then
			table.insert(candidates, gene)
		end
	end
	
	if #candidates == 0 then
		return
	end
	
	local gene = candidates[math.random(1,#candidates)]
	gene.enabled = not gene.enabled
end

--[[ 
Descripción:
  Esta función se utiliza para aplicar varias mutaciones al genoma de un organismo en el contexto de algoritmos genéticos y neuroevolución.
  Las mutaciones pueden incluir la modificación de tasas de mutación y la aplicación de mutaciones específicas, como mutación puntual,
  mutación de enlace, mutación de nodo, habilitar/deshabilitar mutación, entre otras.

Parámetros:
  - genome (tabla): El genoma del organismo al que se le aplicarán las mutaciones.

Retorno:
  Ninguno.
--]]
function mutate(genome)
	for mutation,rate in pairs(genome.mutationRates) do
		if math.random(1,2) == 1 then
			genome.mutationRates[mutation] = 0.95*rate
		else
			genome.mutationRates[mutation] = 1.05263*rate
		end
	end

	if math.random() < genome.mutationRates["connections"] then
		pointMutate(genome)
	end
	
	local p = genome.mutationRates["link"]
	while p > 0 do
		if math.random() < p then
			linkMutate(genome, false)
		end
		p = p - 1
	end

	p = genome.mutationRates["bias"]
	while p > 0 do
		if math.random() < p then
			linkMutate(genome, true)
		end
		p = p - 1
	end
	
	p = genome.mutationRates["node"]
	while p > 0 do
		if math.random() < p then
			nodeMutate(genome)
		end
		p = p - 1
	end
	
	p = genome.mutationRates["enable"]
	while p > 0 do
		if math.random() < p then
			enableDisableMutate(genome, true)
		end
		p = p - 1
	end

	p = genome.mutationRates["disable"]
	while p > 0 do
		if math.random() < p then
			enableDisableMutate(genome, false)
		end
		p = p - 1
	end
end

--[[ 
Descripción:
  Esta función se utiliza para calcular la diferencia de genes (disjoint genes) entre dos conjuntos de genes de genomas en el contexto
  de algoritmos genéticos y neuroevolución. La diferencia de genes mide la cantidad de genes que son exclusivos de uno u otro conjunto
  de genes, lo que puede indicar la divergencia entre genomas.

Parámetros:
  - genes1 (tabla): El primer conjunto de genes (genoma).
  - genes2 (tabla): El segundo conjunto de genes (genoma).

Retorno:
  - Un valor numérico que representa la proporción de genes disjuntos con respecto al número total de genes en ambos conjuntos.
--]]
function disjoint(genes1, genes2)
	local i1 = {}
	for i = 1,#genes1 do
		local gene = genes1[i]
		i1[gene.innovation] = true
	end

	local i2 = {}
	for i = 1,#genes2 do
		local gene = genes2[i]
		i2[gene.innovation] = true
	end
	
	local disjointGenes = 0
	for i = 1,#genes1 do
		local gene = genes1[i]
		if not i2[gene.innovation] then
			disjointGenes = disjointGenes+1
		end
	end
	
	for i = 1,#genes2 do
		local gene = genes2[i]
		if not i1[gene.innovation] then
			disjointGenes = disjointGenes+1
		end
	end
	
	local n = math.max(#genes1, #genes2)
	
	return disjointGenes / n
end

--[[ 
Descripción:
  Esta función se utiliza para calcular la diferencia de pesos (weights difference) entre dos conjuntos de genes de genomas en el contexto de
  algoritmos genéticos y neuroevolución. La diferencia de pesos mide la similitud de los pesos de las conexiones entre los genomas y
  proporciona información sobre cuánto difieren las fuerzas de las conexiones en términos de aprendizaje y funcionamiento de la red neuronal.

Parámetros:
  - genes1 (tabla): El primer conjunto de genes (genoma).
  - genes2 (tabla): El segundo conjunto de genes (genoma).

Retorno:
  - Un valor numérico que representa la diferencia promedio de pesos entre los genes coincidentes en ambos conjuntos.
--]]
function weights(genes1, genes2)
	local i2 = {}
	for i = 1,#genes2 do
		local gene = genes2[i]
		i2[gene.innovation] = gene
	end

	local sum = 0
	local coincident = 0
	for i = 1,#genes1 do
		local gene = genes1[i]
		if i2[gene.innovation] ~= nil then
			local gene2 = i2[gene.innovation]
			sum = sum + math.abs(gene.weight - gene2.weight)
			coincident = coincident + 1
		end
	end
	
	return sum / coincident
end
	
--[[ 
Función: sameSpecies

Descripción:
  Esta función se utiliza para determinar si dos genomas pertenecen a la misma especie en el contexto de algoritmos genéticos y neuroevolución.
  La determinación se basa en la comparación de las diferencias de genes y las diferencias de pesos entre los genomas con umbrales específicos.

Parámetros:
  - genome1 (tabla): El primer genoma a comparar.
  - genome2 (tabla): El segundo genoma a comparar.

Retorno:
  - Un valor booleano que indica si los genomas pertenecen a la misma especie (verdadero) o no (falso).
--]]
function sameSpecies(genome1, genome2)
	local dd = DeltaDisjoint*disjoint(genome1.genes, genome2.genes)
	local dw = DeltaWeights*weights(genome1.genes, genome2.genes) 
	return dd + dw < DeltaThreshold
end

--[[ 
Descripción:
  Esta función se utiliza para calcular y asignar rangos globales a todos los genomas de todas las especies presentes en el conjunto de
  población. Los rangos globales se asignan en función de la aptitud (fitness) de los genomas, donde los genomas con mayor aptitud
  obtienen rangos más bajos y los genomas con menor aptitud obtienen rangos más altos.

Parámetros:
  Ninguno.

Retorno:
  Ninguno.
--]]
function rankGlobally()
	local global = {}
	for s = 1,#pool.species do
		local species = pool.species[s]
		for g = 1,#species.genomes do
			table.insert(global, species.genomes[g])
		end
	end
	table.sort(global, function (a,b)
		return (a.fitness < b.fitness)
	end)
	
	for g=1,#global do
		global[g].globalRank = g
	end
end

--[[ 
Descripción:
  Esta función se utiliza para calcular la aptitud promedio de una especie. La aptitud promedio se calcula sumando los rangos globales
  de los genomas individuales en la especie y dividiendo la suma total por la cantidad de genomas en la especie.

Parámetros:
  - species (tabla): La especie para la cual se calculará la aptitud promedio.

Retorno:
  Ninguno.
--]]
function calculateAverageFitness(species)
	local total = 0
	
	for g=1,#species.genomes do
		local genome = species.genomes[g]
		total = total + genome.globalRank
	end
	
	species.averageFitness = total / #species.genomes
end

--[[ 
Descripción:
  Esta función se utiliza para calcular la aptitud promedio total de todas las especies presentes en el conjunto de población. La aptitud
  promedio total se obtiene sumando las aptitudes promedio de todas las especies.

Parámetros:
  Ninguno.

Retorno:
  - Un valor numérico que representa la aptitud promedio total de todas las especies presentes en el conjunto de población.
--]]
function totalAverageFitness()
	local total = 0
	for s = 1,#pool.species do
		local species = pool.species[s]
		total = total + species.averageFitness
	end

	return total
end

--[[ 
Descripción:
  Esta función se utiliza para reducir el número de genomas en cada especie, manteniendo solo los genomas más aptos. Puede cortar una
  especie a la mitad o mantener solo al genoma más apto de cada especie, dependiendo del valor del parámetro 'cutToOne'.

Parámetros:
  - cutToOne (booleano): Un valor booleano que determina si se debe cortar cada especie a un solo genoma (verdadero) o reducirla a la mitad
    de su tamaño actual (falso).

Retorno:
  Ninguno.
--]]
function cullSpecies(cutToOne)
	for s = 1,#pool.species do
		local species = pool.species[s]
		
		table.sort(species.genomes, function (a,b)
			return (a.fitness > b.fitness)
		end)
		
		local remaining = math.ceil(#species.genomes/2)
		if cutToOne then
			remaining = 1
		end
		while #species.genomes > remaining do
			table.remove(species.genomes)
		end
	end
end

--[[ 
Descripción:
  Esta función se utiliza para crear un nuevo genoma hijo a partir de los genomas de una especie específica. El nuevo genoma hijo puede
  ser generado mediante cruces genéticos (crossover) entre dos genomas existentes de la especie o copiando un genoma existente y aplicando
  mutaciones. La elección entre el cruce y la copia se determina aleatoriamente según la probabilidad de cruce (CrossoverChance).

Parámetros:
  - species (tabla): La especie de la cual se seleccionarán los genomas para generar el genoma hijo.

Retorno:
  - Un nuevo genoma (tabla) que representa al genoma hijo.
--]]
function breedChild(species)
	local child = {}
	if math.random() < CrossoverChance then
		g1 = species.genomes[math.random(1, #species.genomes)]
		g2 = species.genomes[math.random(1, #species.genomes)]
		child = crossover(g1, g2)
	else
		g = species.genomes[math.random(1, #species.genomes)]
		child = copyGenome(g)
	end
	
	mutate(child)
	
	return child
end

--[[ 
Descripción:
  Esta función se utiliza para eliminar las especies estancadas o "stale" del conjunto de población. Una especie se considera estancada si
  su genoma más apto no ha mejorado su aptitud durante un número determinado de generaciones (StaleSpecies), o si la aptitud de su genoma
  más apto alcanza o supera la máxima aptitud global de la población (pool.maxFitness).

Parámetros:
  Ninguno.

Retorno:
  Ninguno.
--]]
function removeStaleSpecies()
	local survived = {}

	for s = 1,#pool.species do
		local species = pool.species[s]
		
		table.sort(species.genomes, function (a,b)
			return (a.fitness > b.fitness)
		end)
		
		if species.genomes[1].fitness > species.topFitness then
			species.topFitness = species.genomes[1].fitness
			species.staleness = 0
		else
			species.staleness = species.staleness + 1
		end
		if species.staleness < StaleSpecies or species.topFitness >= pool.maxFitness then
			table.insert(survived, species)
		end
	end

	pool.species = survived
end

--[[ 
Descripción:
  Esta función se utiliza para eliminar las especies más débiles del conjunto de población en función de su aptitud relativa en comparación con
  la aptitud total de la población. Las especies con una aptitud promedio por debajo de un cierto umbral no sobrevivirán en la población
  final.

Parámetros:
  Ninguno.

Retorno:
  Ninguno.
--]]
function removeWeakSpecies()
	local survived = {}

	local sum = totalAverageFitness()
	for s = 1,#pool.species do
		local species = pool.species[s]
		breed = math.floor(species.averageFitness / sum * Population)
		if breed >= 1 then
			table.insert(survived, species)
		end
	end

	pool.species = survived
end

--[[ 
Descripción:
  Esta función se utiliza para asignar un genoma hijo a una especie existente o crear una nueva especie si el hijo no coincide con ninguna
  especie existente en función de la similitud genética. La similitud genética se determina mediante la función 'sameSpecies', que compara el
  genoma hijo con el genoma líder de cada especie en el conjunto de población.

Parámetros:
  - child: El genoma hijo que se va a agregar a una especie existente o nueva.

Retorno:
  Ninguno.
--]]
function addToSpecies(child)
	local foundSpecies = false
	for s=1,#pool.species do
		local species = pool.species[s]
		if not foundSpecies and sameSpecies(child, species.genomes[1]) then
			table.insert(species.genomes, child)
			foundSpecies = true
		end
	end
	
	if not foundSpecies then
		local childSpecies = newSpecies()
		table.insert(childSpecies.genomes, child)
		table.insert(pool.species, childSpecies)
	end
end

--[[ 
Descripción:
  Esta función se utiliza para generar una nueva generación de genomas mediante el proceso de selección, reproducción y mutación. La población
  actual se somete a varios pasos, como la eliminación de las especies más débiles, el cálculo de la aptitud promedio, la eliminación de
  especies estancadas y la creación de una nueva generación de genomas a partir de las especies más aptas. Finalmente, la función guarda una
  copia de seguridad de la población actual.

Parámetros:
  Ninguno.

Retorno:
  Ninguno.
--]]
function newGeneration()
	cullSpecies(false) -- Cull the bottom half of each species
	rankGlobally()
	removeStaleSpecies()
	rankGlobally()
	for s = 1,#pool.species do
		local species = pool.species[s]
		calculateAverageFitness(species)
	end
	removeWeakSpecies()
	local sum = totalAverageFitness()
	local children = {}
	for s = 1,#pool.species do
		local species = pool.species[s]
		breed = math.floor(species.averageFitness / sum * Population) - 1
		for i=1,breed do
			table.insert(children, breedChild(species))
		end
	end
	cullSpecies(true) -- Cull all but the top member of each species
	while #children + #pool.species < Population do
		local species = pool.species[math.random(1, #pool.species)]
		table.insert(children, breedChild(species))
	end
	for c=1,#children do
		local child = children[c]
		addToSpecies(child)
	end
	
	pool.generation = pool.generation + 1
	
	writeFile("backup." .. pool.generation .. "." .. forms.gettext(saveLoadFile))
end

--[[ 
Descripción:
  Esta función se utiliza para inicializar la piscina (pool) de genomas y la población inicial. Crea una nueva piscina, genera genomas básicos y
  los asigna a especies, y luego inicializa una corrida (run) utilizando la función 'initializeRun'.

Parámetros:
  Ninguno.

Retorno:
  Ninguno.
--]]
function initializePool()
	pool = newPool()

	for i=1,Population do
		basic = basicGenome()
		addToSpecies(basic)
	end

	initializeRun()
end

--[[ 
Descripción:
  Esta función se utiliza para limpiar el estado del controlador (joypad) estableciendo todas las entradas de botones en falso. 
  Esto significa que ningún botón del controlador está presionado.

Parámetros:
  Ninguno.

Retorno:
  Ninguno.
--]]
function clearJoypad()
	controller = {}
	for b = 1,#ButtonNames do
		controller["P1 " .. ButtonNames[b]] = false
	end
	joypad.set(controller)
end

--[[ 
Descripción:
  Esta función se utiliza para inicializar una nueva corrida (run) en un entorno de emulación de juegos. Carga un estado previamente guardado,
  establece algunas variables de seguimiento y evalúa el genoma actual en el entorno del juego.

Parámetros:
  Ninguno.

Retorno:
  Ninguno.
--]]
function initializeRun()
	savestate.load(Filename);
	rightmost = 0
	pool.currentFrame = 0
	timeout = TimeoutConstant
	clearJoypad()
	
	local species = pool.species[pool.currentSpecies]
	local genome = species.genomes[pool.currentGenome]
	generateNetwork(genome)
	evaluateCurrent()
end

--[[ 
Descripción:
  Esta función se utiliza para evaluar el genoma actual en el entorno del juego. Obtiene las entradas del genoma actual utilizando 'getInputs',
  utiliza la red neuronal del genoma para calcular las salidas del controlador y aplica estas salidas al controlador del juego.

Parámetros:
  Ninguno.

Retorno:
  Ninguno.
--]]
function evaluateCurrent()
	local species = pool.species[pool.currentSpecies]
	local genome = species.genomes[pool.currentGenome]

	inputs = getInputs()
	controller = evaluateNetwork(genome.network, inputs)
	
	if controller["P1 Left"] and controller["P1 Right"] then
		controller["P1 Left"] = false
		controller["P1 Right"] = false
	end
	if controller["P1 Up"] and controller["P1 Down"] then
		controller["P1 Up"] = false
		controller["P1 Down"] = false
	end

	joypad.set(controller)
end

if pool == nil then
	initializePool()
end

--[[ 
Descripción:
  Esta función se utiliza para avanzar al próximo genoma en la evaluación actual del juego. 
  Cambia el enfoque del genoma actual al siguiente en la especie actual. 
  Si se han evaluado todos los genomas de la especie actual, se pasa a la siguiente especie en el pool.
  Si se han evaluado todos los genomas de todas las especies, se genera una nueva generación.

Parámetros:
  Ninguno.

Retorno:
  Ninguno.
--]]
function nextGenome()
	pool.currentGenome = pool.currentGenome + 1
	if pool.currentGenome > #pool.species[pool.currentSpecies].genomes then
		pool.currentGenome = 1
		pool.currentSpecies = pool.currentSpecies+1
		if pool.currentSpecies > #pool.species then
			newGeneration()
			pool.currentSpecies = 1
		end
	end
end

--[[ 
Descripción:
  Esta función se utiliza para determinar si la aptitud (fitness) de un genoma ya ha sido medida o no. 
  Comprueba si el valor de aptitud del genoma actual es distinto de cero, lo que indica que la aptitud ya ha sido evaluada.

Parámetros:
  Ninguno.

Retorno:
  - Un valor booleano (true o false) que indica si la aptitud del genoma actual ya ha sido medida (true) o no (false).
--]]
function fitnessAlreadyMeasured()
	local species = pool.species[pool.currentSpecies]
	local genome = species.genomes[pool.currentGenome]
	
	return genome.fitness ~= 0
end

--[[ 
Descripción:
  Esta función se utiliza para mostrar visualmente la red neuronal y la estructura del genoma en una interfaz gráfica.
  Dibuja las neuronas y las conexiones entre ellas en una ventana gráfica y muestra información sobre el valor y el peso de las conexiones.
  También muestra las mutaciones aplicadas en forma de colores en las conexiones.

Parámetros:
  - genome: El genoma que se va a visualizar.

Retorno:
  Ninguno.
--]]
function displayGenome(genome)
	local network = genome.network
	local cells = {}
	local i = 1
	local cell = {}
	for dy=-BoxRadius,BoxRadius do
		for dx=-BoxRadius,BoxRadius do
			cell = {}
			cell.x = 50+5*dx
			cell.y = 70+5*dy
			cell.value = network.neurons[i].value
			cells[i] = cell
			i = i + 1
		end
	end
	local biasCell = {}
	biasCell.x = 80
	biasCell.y = 110
	biasCell.value = network.neurons[Inputs].value
	cells[Inputs] = biasCell
	
	for o = 1,Outputs do
		cell = {}
		cell.x = 220
		cell.y = 30 + 8 * o
		cell.value = network.neurons[MaxNodes + o].value
		cells[MaxNodes+o] = cell
		local color
		if cell.value > 0 then
			color = 0xFF0000FF
		else
			color = 0xFF000000
		end
		gui.drawText(223, 24+8*o, ButtonNames[o], color, 9)
	end
	
	for n,neuron in pairs(network.neurons) do
		cell = {}
		if n > Inputs and n <= MaxNodes then
			cell.x = 140
			cell.y = 40
			cell.value = neuron.value
			cells[n] = cell
		end
	end
	
	for n=1,4 do
		for _,gene in pairs(genome.genes) do
			if gene.enabled then
				local c1 = cells[gene.into]
				local c2 = cells[gene.out]
				if gene.into > Inputs and gene.into <= MaxNodes then
					c1.x = 0.75*c1.x + 0.25*c2.x
					if c1.x >= c2.x then
						c1.x = c1.x - 40
					end
					if c1.x < 90 then
						c1.x = 90
					end
					
					if c1.x > 220 then
						c1.x = 220
					end
					c1.y = 0.75*c1.y + 0.25*c2.y
					
				end
				if gene.out > Inputs and gene.out <= MaxNodes then
					c2.x = 0.25*c1.x + 0.75*c2.x
					if c1.x >= c2.x then
						c2.x = c2.x + 40
					end
					if c2.x < 90 then
						c2.x = 90
					end
					if c2.x > 220 then
						c2.x = 220
					end
					c2.y = 0.25*c1.y + 0.75*c2.y
				end
			end
		end
	end
	
	gui.drawBox(50-BoxRadius*5-3,70-BoxRadius*5-3,50+BoxRadius*5+2,70+BoxRadius*5+2,0xFF000000, 0x80808080)
	for n,cell in pairs(cells) do
		if n > Inputs or cell.value ~= 0 then
			local color = math.floor((cell.value+1)/2*256)
			if color > 255 then color = 255 end
			if color < 0 then color = 0 end
			local opacity = 0xFF000000
			if cell.value == 0 then
				opacity = 0x50000000
			end
			color = opacity + color*0x10000 + color*0x100 + color
			gui.drawBox(cell.x-2,cell.y-2,cell.x+2,cell.y+2,opacity,color)
		end
	end
	for _,gene in pairs(genome.genes) do
		if gene.enabled then
			local c1 = cells[gene.into]
			local c2 = cells[gene.out]
			local opacity = 0xA0000000
			if c1.value == 0 then
				opacity = 0x20000000
			end
			
			local color = 0x80-math.floor(math.abs(sigmoid(gene.weight))*0x80)
			if gene.weight > 0 then 
				color = opacity + 0x8000 + 0x10000*color
			else
				color = opacity + 0x800000 + 0x100*color
			end
			gui.drawLine(c1.x+1, c1.y, c2.x-3, c2.y, color)
		end
	end
	
	gui.drawBox(49,71,51,78,0x00000000,0x80FF0000)
	
	if forms.ischecked(showMutationRates) then
		local pos = 100
		for mutation,rate in pairs(genome.mutationRates) do
			gui.drawText(100, pos, mutation .. ": " .. rate, 0xFF000000, 10)
			pos = pos + 8
		end
	end
end

--[[ 
Descripción:
  Esta función se utiliza para escribir datos de configuración y genéticos en un archivo. Los datos incluyen información sobre generaciones, fitness, especies, genomas y tasas de mutación.
  
Parámetros:
  - filename: El nombre del archivo en el que se guardarán los datos.

Retorno:
  Ninguno.
--]]
function writeFile(filename)
        local file = io.open(filename, "w")
	file:write(pool.generation .. "\n")
	file:write(pool.maxFitness .. "\n")
	file:write(#pool.species .. "\n")
        for n,species in pairs(pool.species) do
		file:write(species.topFitness .. "\n")
		file:write(species.staleness .. "\n")
		file:write(#species.genomes .. "\n")
		for m,genome in pairs(species.genomes) do
			file:write(genome.fitness .. "\n")
			file:write(genome.maxneuron .. "\n")
			for mutation,rate in pairs(genome.mutationRates) do
				file:write(mutation .. "\n")
				file:write(rate .. "\n")
			end
			file:write("done\n")
			
			file:write(#genome.genes .. "\n")
			for l,gene in pairs(genome.genes) do
				file:write(gene.into .. " ")
				file:write(gene.out .. " ")
				file:write(gene.weight .. " ")
				file:write(gene.innovation .. " ")
				if(gene.enabled) then
					file:write("1\n")
				else
					file:write("0\n")
				end
			end
		end
        end
        file:close()
end

--[[ 
Descripción:
  Esta función se utiliza para guardar el estado actual de la piscina genética en un archivo. El nombre del archivo se toma de un cuadro de texto de formulario llamado 'saveLoadFile'. Los datos de la piscina, incluyendo generaciones, fitness, especies, genomas y tasas de mutación, se guardan en el archivo especificado.

Parámetros:
  Ninguno.

Retorno:
  Ninguno.
--]]
function savePool()
	local filename = forms.gettext(saveLoadFile)
	writeFile(filename)
end

--[[ 
Descripción:
  Esta función se utiliza para cargar un estado de la piscina genética desde un archivo. El nombre del archivo se pasa como parámetro (filename). Los datos de la piscina, incluyendo generaciones, fitness, especies, genomas y tasas de mutación, se cargan desde el archivo especificado.

Parámetros:
  - filename (cadena): El nombre del archivo desde el que se cargará el estado de la piscina.

Retorno:
  Ninguno.
--]]
function loadFile(filename)
        local file = io.open(filename, "r")
	pool = newPool()
	pool.generation = file:read("*number")
	pool.maxFitness = file:read("*number")
	forms.settext(maxFitnessLabel, "Max Fitness: " .. math.floor(pool.maxFitness))
        local numSpecies = file:read("*number")
        for s=1,numSpecies do
		local species = newSpecies()
		table.insert(pool.species, species)
		species.topFitness = file:read("*number")
		species.staleness = file:read("*number")
		local numGenomes = file:read("*number")
		for g=1,numGenomes do
			local genome = newGenome()
			table.insert(species.genomes, genome)
			genome.fitness = file:read("*number")
			genome.maxneuron = file:read("*number")
			local line = file:read("*line")
			while line ~= "done" do
				genome.mutationRates[line] = file:read("*number")
				line = file:read("*line")
			end
			local numGenes = file:read("*number")
			for n=1,numGenes do
				local gene = newGene()
				table.insert(genome.genes, gene)
				local enabled
				gene.into, gene.out, gene.weight, gene.innovation, enabled = file:read("*number", "*number", "*number", "*number", "*number")
				if enabled == 0 then
					gene.enabled = false
				else
					gene.enabled = true
				end
				
			end
		end
	end
        file:close()
	
	while fitnessAlreadyMeasured() do
		nextGenome()
	end
	initializeRun()
	pool.currentFrame = pool.currentFrame + 1
end
 
--[[ 
Descripción:
  Esta función se utiliza para cargar el estado de la piscina genética desde un archivo utilizando el nombre de archivo proporcionado en un campo de texto en el formulario. Llama a la función `loadFile(filename)` para cargar los datos del archivo.

Parámetros:
  Ninguno.

Retorno:
  Ninguno.
--]]
function loadPool()
	local filename = forms.gettext(saveLoadFile)
	loadFile(filename)
end

--[[ 
Descripción:
  Esta función se utiliza para seleccionar y reproducir el mejor genoma dentro de la piscina genética. Encuentra el genoma con la mayor aptitud y lo establece como el genoma actual para jugar.

Parámetros:
  Ninguno.

Retorno:
  Ninguno.
--]]
function playTop()
	local maxfitness = 0
	local maxs, maxg
	for s,species in pairs(pool.species) do
		for g,genome in pairs(species.genomes) do
			if genome.fitness > maxfitness then
				maxfitness = genome.fitness
				maxs = s
				maxg = g
			end
		end
	end
	
	pool.currentSpecies = maxs
	pool.currentGenome = maxg
	pool.maxFitness = maxfitness
	forms.settext(maxFitnessLabel, "Max Fitness: " .. math.floor(pool.maxFitness))
	initializeRun()
	pool.currentFrame = pool.currentFrame + 1
	return
end

-- Esta función se ejecuta cuando el programa o script se está cerrando o finalizando.
function onExit()
    -- Destruye el formulario (interfaz gráfica) llamado "form".
    forms.destroy(form)
end


-- Guarda el estado actual de la población en un archivo llamado "temp.pool".
writeFile("temp.pool")

-- Define una función que se ejecutará cuando el programa se cierre.
event.onexit(onExit)

-- Crea una nueva ventana de formulario con un tamaño de 200x260 píxeles y un título "Fitness".
form = forms.newform(200, 260, "Fitness")

-- Crea una etiqueta que muestra la "Max Fitness" actual en la ventana del formulario.
maxFitnessLabel = forms.label(form, "Max Fitness: " .. math.floor(pool.maxFitness), 5, 8)

-- Crea una casilla de verificación para habilitar o deshabilitar la visualización del mapa.
showNetwork = forms.checkbox(form, "Show Map", 5, 30)

-- Crea una casilla de verificación para habilitar o deshabilitar la visualización de las tasas de mutación.
showMutationRates = forms.checkbox(form, "Show M-Rates", 5, 52)

-- Crea un botón "Restart" que ejecuta la función "initializePool" al hacer clic.
restartButton = forms.button(form, "Restart", initializePool, 5, 77)

-- Crea un botón "Save" que ejecuta la función "savePool" al hacer clic.
saveButton = forms.button(form, "Save", savePool, 5, 102)

-- Crea un botón "Load" que ejecuta la función "loadPool" al hacer clic.
loadButton = forms.button(form, "Load", loadPool, 80, 102)

-- Crea un cuadro de texto para ingresar el nombre del archivo de guardado/carga.
saveLoadFile = forms.textbox(form, Filename .. ".pool", 170, 25, nil, 5, 148)

-- Crea una etiqueta para indicar la funcionalidad de guardado/carga.
saveLoadLabel = forms.label(form, "Save/Load:", 5, 129)

-- Crea un botón "Play Top" que ejecuta la función "playTop" al hacer clic.
playTopButton = forms.button(form, "Play Top", playTop, 5, 170)

-- Crea una casilla de verificación para ocultar o mostrar un banner en la interfaz gráfica.
hideBanner = forms.checkbox(form, "Hide Banner", 5, 190)



-- Este bucle es el bucle principal del programa y se ejecuta indefinidamente.
while true do
	-- Configuración del color de fondo para la interfaz gráfica.
	local backgroundColor = 0xD0FFFFFF
	
	-- Comprueba si se debe ocultar el banner y, si no, dibuja un cuadro en la pantalla.
	if not forms.ischecked(hideBanner) then
		gui.drawBox(0, 0, 300, 26, backgroundColor, backgroundColor)
	end

	-- Obtiene información sobre la especie y genoma actual.
	local species = pool.species[pool.currentSpecies]
	local genome = species.genomes[pool.currentGenome]

	-- Muestra la red neuronal del genoma si la opción está habilitada.
	if forms.ischecked(showNetwork) then
		displayGenome(genome)
	end

	-- Evalúa el rendimiento en el juego cada 5 fotogramas.
	if pool.currentFrame % 5 == 0 then
		evaluateCurrent()
	end

	-- Configura el controlador del juego.
	joypad.set(controller)

	-- Obtiene las posiciones en el juego.
	getPositions()

	-- Actualiza la posición más a la derecha alcanzada por el personaje.
	if marioX > rightmost then
		rightmost = marioX
		timeout = TimeoutConstant
	end

	-- Reduce el contador de tiempo de espera.
	timeout = timeout - 1

	-- Calcula un bono de tiempo de espera basado en el fotograma actual.
	local timeoutBonus = pool.currentFrame / 4

	-- Calcula la aptitud del genoma en función de la posición alcanzada y el tiempo de espera.
	if timeout + timeoutBonus <= 0 then
		local fitness = rightmost - pool.currentFrame / 2
		
		-- Añade un bono de aptitud si se alcanza una posición específica en el juego.
		if gameinfo.getromname() == "Super Mario World (USA)" and rightmost > 4816 then
			fitness = fitness + 1000
		end
		if gameinfo.getromname() == "Super Mario Bros." and rightmost > 3186 then
			fitness = fitness + 1000
		end
		
		-- Si la aptitud es cero, establece un valor negativo.
		if fitness == 0 then
			fitness = -1
		end

		-- Asigna la aptitud calculada al genoma actual.
		genome.fitness = fitness

		-- Actualiza la máxima aptitud registrada en la población.
		if fitness > pool.maxFitness then
			pool.maxFitness = fitness
			forms.settext(maxFitnessLabel, "Max Fitness: " .. math.floor(pool.maxFitness))
			writeFile("backup." .. pool.generation .. "." .. forms.gettext(saveLoadFile))
		end

		-- Imprime información sobre la aptitud del genoma actual en la consola.
		console.writeline("Gen " .. pool.generation .. " species " .. pool.currentSpecies .. " genome " .. pool.currentGenome .. " fitness: " .. fitness)

		-- Reinicia el contador de especies y genomas si la aptitud ya ha sido evaluada.
		pool.currentSpecies = 1
		pool.currentGenome = 1
		while fitnessAlreadyMeasured() do
			nextGenome()
		end

		-- Inicializa una nueva ejecución del juego.
		initializeRun()
	end

	-- Calcula la cantidad de genomas evaluados y el total de genomas en la población.
	local measured = 0
	local total = 0
	for _, species in pairs(pool.species) do
		for _, genome in pairs(species.genomes) do
			total = total + 1
			if genome.fitness ~= 0 then
				measured = measured + 1
			end
		end
	end

	-- Muestra información en la interfaz gráfica si el banner no está oculto.
	if not forms.ischecked(hideBanner) then
		gui.drawText(0, 0, "Gen " .. pool.generation .. " species " .. pool.currentSpecies .. " genome " .. pool.currentGenome .. " (" .. math.floor(measured / total * 100) .. "%)", 0xFF000000, 11)
		gui.drawText(0, 12, "Fitness: " .. math.floor(rightmost - (pool.currentFrame) / 2 - (timeout + timeoutBonus) * 2 / 3), 0xFF000000, 11)
		gui.drawText(100, 12, "Max Fitness: " .. math.floor(pool.maxFitness), 0xFF000000, 11)
	end

	-- Actualiza el contador de fotogramas.
	pool.currentFrame = pool.currentFrame + 1

	-- Avanza un fotograma en la emulación.
	emu.frameadvance()
end