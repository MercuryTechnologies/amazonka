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
-- Module      : Amazonka.XRay.Types.ServiceId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ServiceId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newServiceId' smart constructor.
data ServiceId = ServiceId'
  { accountId :: Prelude.Maybe Prelude.Text,
    names :: Prelude.Maybe [Prelude.Text],
    name :: Prelude.Maybe Prelude.Text,
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'serviceId_accountId' -
--
-- 'names', 'serviceId_names' -
--
-- 'name', 'serviceId_name' -
--
-- 'type'', 'serviceId_type' -
newServiceId ::
  ServiceId
newServiceId =
  ServiceId'
    { accountId = Prelude.Nothing,
      names = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- |
serviceId_accountId :: Lens.Lens' ServiceId (Prelude.Maybe Prelude.Text)
serviceId_accountId = Lens.lens (\ServiceId' {accountId} -> accountId) (\s@ServiceId' {} a -> s {accountId = a} :: ServiceId)

-- |
serviceId_names :: Lens.Lens' ServiceId (Prelude.Maybe [Prelude.Text])
serviceId_names = Lens.lens (\ServiceId' {names} -> names) (\s@ServiceId' {} a -> s {names = a} :: ServiceId) Prelude.. Lens.mapping Lens.coerced

-- |
serviceId_name :: Lens.Lens' ServiceId (Prelude.Maybe Prelude.Text)
serviceId_name = Lens.lens (\ServiceId' {name} -> name) (\s@ServiceId' {} a -> s {name = a} :: ServiceId)

-- |
serviceId_type :: Lens.Lens' ServiceId (Prelude.Maybe Prelude.Text)
serviceId_type = Lens.lens (\ServiceId' {type'} -> type') (\s@ServiceId' {} a -> s {type' = a} :: ServiceId)

instance Core.FromJSON ServiceId where
  parseJSON =
    Core.withObject
      "ServiceId"
      ( \x ->
          ServiceId'
            Prelude.<$> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "Names" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable ServiceId where
  hashWithSalt salt' ServiceId' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData ServiceId where
  rnf ServiceId' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf names
