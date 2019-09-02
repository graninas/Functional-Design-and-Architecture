package ServerContext {
	object Connection {
		def send(name: String, dataType: String, v: Float) {
			println(s"Sended: $name $dataType $v")
		}
	}
}
