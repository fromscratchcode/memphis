use crate::{
    core::net::Connection,
    treewalk::{
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
        macros::impl_method_provider,
        protocols::Callable,
        result::Raise,
        types::Exception,
        utils::{BoundArgs, Signature},
    },
};

impl_method_provider!(Connection, [ConnRecv, ConnSend, ConnClose,]);

#[derive(Clone)]
struct ConnRecv;
#[derive(Clone)]
struct ConnSend;
#[derive(Clone)]
struct ConnClose;

impl Callable for ConnRecv {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "bytes"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let n = args.get("bytes").as_int().raise(interpreter)?;

        let mut conn = args
            .get("self")
            .as_native_object_mut::<Connection>()
            .raise(interpreter)?;
        let bytes = conn
            .recv(n as usize)
            .map_err(|e| Exception::runtime_error_with(format!("Connection.recv() failed: {}", e)))
            .raise(interpreter)?;

        Ok(TreewalkValue::Bytes(bytes))
    }

    fn name(&self) -> String {
        "recv".into()
    }
}

impl Callable for ConnSend {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "data"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let data = args.get("data").as_bytes().raise(interpreter)?;

        let mut conn = args
            .get("self")
            .as_native_object_mut::<Connection>()
            .raise(interpreter)?;
        conn.send(&data)
            .map_err(|e| Exception::runtime_error_with(format!("Connection.send() failed: {}", e)))
            .raise(interpreter)?;

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "send".into()
    }
}

impl Callable for ConnClose {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let mut conn = args
            .get("self")
            .as_native_object_mut::<Connection>()
            .raise(interpreter)?;
        conn.close()
            .map_err(|e| Exception::runtime_error_with(format!("Connection.close() failed: {}", e)))
            .raise(interpreter)?;

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "close".into()
    }
}
