{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DynamoDB.Types.TransactWriteItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.TransactWriteItem where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Types.ConditionCheck
import Amazonka.DynamoDB.Types.Delete
import Amazonka.DynamoDB.Types.Put
import Amazonka.DynamoDB.Types.Update
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of requests that can perform update, put, delete, or check
-- operations on multiple items in one or more tables atomically.
--
-- /See:/ 'newTransactWriteItem' smart constructor.
data TransactWriteItem = TransactWriteItem'
  { -- | A request to perform a check item operation.
    conditionCheck :: Prelude.Maybe ConditionCheck,
    -- | A request to perform a @PutItem@ operation.
    put :: Prelude.Maybe Put,
    -- | A request to perform a @DeleteItem@ operation.
    delete' :: Prelude.Maybe Delete,
    -- | A request to perform an @UpdateItem@ operation.
    update :: Prelude.Maybe Update
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransactWriteItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionCheck', 'transactWriteItem_conditionCheck' - A request to perform a check item operation.
--
-- 'put', 'transactWriteItem_put' - A request to perform a @PutItem@ operation.
--
-- 'delete'', 'transactWriteItem_delete' - A request to perform a @DeleteItem@ operation.
--
-- 'update', 'transactWriteItem_update' - A request to perform an @UpdateItem@ operation.
newTransactWriteItem ::
  TransactWriteItem
newTransactWriteItem =
  TransactWriteItem'
    { conditionCheck =
        Prelude.Nothing,
      put = Prelude.Nothing,
      delete' = Prelude.Nothing,
      update = Prelude.Nothing
    }

-- | A request to perform a check item operation.
transactWriteItem_conditionCheck :: Lens.Lens' TransactWriteItem (Prelude.Maybe ConditionCheck)
transactWriteItem_conditionCheck = Lens.lens (\TransactWriteItem' {conditionCheck} -> conditionCheck) (\s@TransactWriteItem' {} a -> s {conditionCheck = a} :: TransactWriteItem)

-- | A request to perform a @PutItem@ operation.
transactWriteItem_put :: Lens.Lens' TransactWriteItem (Prelude.Maybe Put)
transactWriteItem_put = Lens.lens (\TransactWriteItem' {put} -> put) (\s@TransactWriteItem' {} a -> s {put = a} :: TransactWriteItem)

-- | A request to perform a @DeleteItem@ operation.
transactWriteItem_delete :: Lens.Lens' TransactWriteItem (Prelude.Maybe Delete)
transactWriteItem_delete = Lens.lens (\TransactWriteItem' {delete'} -> delete') (\s@TransactWriteItem' {} a -> s {delete' = a} :: TransactWriteItem)

-- | A request to perform an @UpdateItem@ operation.
transactWriteItem_update :: Lens.Lens' TransactWriteItem (Prelude.Maybe Update)
transactWriteItem_update = Lens.lens (\TransactWriteItem' {update} -> update) (\s@TransactWriteItem' {} a -> s {update = a} :: TransactWriteItem)

instance Prelude.Hashable TransactWriteItem where
  hashWithSalt salt' TransactWriteItem' {..} =
    salt' `Prelude.hashWithSalt` update
      `Prelude.hashWithSalt` delete'
      `Prelude.hashWithSalt` put
      `Prelude.hashWithSalt` conditionCheck

instance Prelude.NFData TransactWriteItem where
  rnf TransactWriteItem' {..} =
    Prelude.rnf conditionCheck
      `Prelude.seq` Prelude.rnf update
      `Prelude.seq` Prelude.rnf delete'
      `Prelude.seq` Prelude.rnf put

instance Core.ToJSON TransactWriteItem where
  toJSON TransactWriteItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ConditionCheck" Core..=)
              Prelude.<$> conditionCheck,
            ("Put" Core..=) Prelude.<$> put,
            ("Delete" Core..=) Prelude.<$> delete',
            ("Update" Core..=) Prelude.<$> update
          ]
      )
