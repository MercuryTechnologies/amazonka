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
-- Module      : Amazonka.EKS.Types.Update
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.Update where

import qualified Amazonka.Core as Core
import Amazonka.EKS.Types.ErrorDetail
import Amazonka.EKS.Types.UpdateParam
import Amazonka.EKS.Types.UpdateStatus
import Amazonka.EKS.Types.UpdateType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object representing an asynchronous update.
--
-- /See:/ 'newUpdate' smart constructor.
data Update = Update'
  { -- | The current status of the update.
    status :: Prelude.Maybe UpdateStatus,
    -- | The Unix epoch timestamp in seconds for when the update was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | A key-value map that contains the parameters associated with the update.
    params :: Prelude.Maybe [UpdateParam],
    -- | A UUID that is used to track the update.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of the update.
    type' :: Prelude.Maybe UpdateType,
    -- | Any errors associated with a @Failed@ update.
    errors :: Prelude.Maybe [ErrorDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Update' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'update_status' - The current status of the update.
--
-- 'createdAt', 'update_createdAt' - The Unix epoch timestamp in seconds for when the update was created.
--
-- 'params', 'update_params' - A key-value map that contains the parameters associated with the update.
--
-- 'id', 'update_id' - A UUID that is used to track the update.
--
-- 'type'', 'update_type' - The type of the update.
--
-- 'errors', 'update_errors' - Any errors associated with a @Failed@ update.
newUpdate ::
  Update
newUpdate =
  Update'
    { status = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      params = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing,
      errors = Prelude.Nothing
    }

-- | The current status of the update.
update_status :: Lens.Lens' Update (Prelude.Maybe UpdateStatus)
update_status = Lens.lens (\Update' {status} -> status) (\s@Update' {} a -> s {status = a} :: Update)

-- | The Unix epoch timestamp in seconds for when the update was created.
update_createdAt :: Lens.Lens' Update (Prelude.Maybe Prelude.UTCTime)
update_createdAt = Lens.lens (\Update' {createdAt} -> createdAt) (\s@Update' {} a -> s {createdAt = a} :: Update) Prelude.. Lens.mapping Core._Time

-- | A key-value map that contains the parameters associated with the update.
update_params :: Lens.Lens' Update (Prelude.Maybe [UpdateParam])
update_params = Lens.lens (\Update' {params} -> params) (\s@Update' {} a -> s {params = a} :: Update) Prelude.. Lens.mapping Lens.coerced

-- | A UUID that is used to track the update.
update_id :: Lens.Lens' Update (Prelude.Maybe Prelude.Text)
update_id = Lens.lens (\Update' {id} -> id) (\s@Update' {} a -> s {id = a} :: Update)

-- | The type of the update.
update_type :: Lens.Lens' Update (Prelude.Maybe UpdateType)
update_type = Lens.lens (\Update' {type'} -> type') (\s@Update' {} a -> s {type' = a} :: Update)

-- | Any errors associated with a @Failed@ update.
update_errors :: Lens.Lens' Update (Prelude.Maybe [ErrorDetail])
update_errors = Lens.lens (\Update' {errors} -> errors) (\s@Update' {} a -> s {errors = a} :: Update) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Update where
  parseJSON =
    Core.withObject
      "Update"
      ( \x ->
          Update'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "params" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "errors" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Update where
  hashWithSalt salt' Update' {..} =
    salt' `Prelude.hashWithSalt` errors
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` params
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` status

instance Prelude.NFData Update where
  rnf Update' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf params
      `Prelude.seq` Prelude.rnf createdAt
