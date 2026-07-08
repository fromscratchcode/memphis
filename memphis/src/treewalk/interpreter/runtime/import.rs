use crate::{
    core::Container,
    domain::{Identifier, LoadedModule, ModuleName},
    treewalk::{
        DomainResult, TreewalkContext, TreewalkDisruption, TreewalkInterpreter, TreewalkResult,
        TreewalkValue, import_utils,
        result::Raise,
        types::{Exception, Module},
    },
};

impl TreewalkInterpreter {
    pub fn load_module(&self, module_name: &ModuleName) -> TreewalkResult<TreewalkValue> {
        if let Some(module) = self.state.fetch_module(module_name) {
            return Ok(TreewalkValue::Module(module));
        }

        let module = self.import_module(module_name)?;
        Ok(TreewalkValue::Module(module))
    }

    pub fn execute_import(
        &self,
        module_name: &ModuleName,
        module: TreewalkValue,
        alias: &Option<Identifier>,
    ) -> TreewalkResult<()> {
        // This is a case where it's simpler if we have an alias: just make the module available
        // at the alias.
        if let Some(alias) = alias {
            self.store_var(alias.as_str(), module);
        } else {
            // Otherwise, we must create a module chain. For example:
            //
            // import mypackage.myothermodule
            //
            // must be used as
            //
            // mypackage.myothermodule.add('1', '1')
            let outer_module = import_utils::build_module_chain(module_name, module);
            let symbol_name = module_name.head();
            self.store_var(symbol_name, outer_module);
        }

        Ok(())
    }

    fn prepare_imported_module(&self, loaded: &LoadedModule) -> Container<Module> {
        let module = Container::new(Module::new_file_backed(
            loaded.name.clone(),
            loaded.package.clone(),
            loaded.source.path().clone(),
        ));

        // Before we parse and evaluate this module, store an empty module as a placeholder. This
        // is necessary to indicate to downstream modules that the upstream module which called
        // them but hasn't yet finished importing is in progress. Without this, you would get an
        // infinite loop from any circular imports.
        //
        // We don't need to store again after evaluating this module because the object pushed onto
        // the module stack during execution uses `Container<_>` and refers to this same module.
        self.state.store_module(module.clone());

        module
    }

    fn enter_imported_module(&self, module: Container<Module>) {
        self.memphis_state.save_line_number();
        self.memphis_state.push_stack_frame(&*module.borrow());
        self.state.push_module(module);
    }

    fn exit_imported_module(&self) -> DomainResult<Container<Module>> {
        self.memphis_state
            .pop_stack_frame()
            .ok_or_else(Exception::runtime_error)?;
        self.state.pop_module().ok_or_else(Exception::runtime_error)
    }

    fn import_module(&self, module_name: &ModuleName) -> TreewalkResult<Container<Module>> {
        let loaded = self
            .memphis_state
            .load_source(module_name)
            .map_err(|_| Exception::import_error(module_name))
            .raise(self)?;

        let module = self.prepare_imported_module(&loaded);
        self.enter_imported_module(module);

        TreewalkContext::from_state(self.memphis_state.clone(), self.state.clone())
            .eval_inner(loaded.source.text().clone())
            .map_err(TreewalkDisruption::Error)?;

        let module = self.exit_imported_module().raise(self)?;
        Ok(module)
    }
}
