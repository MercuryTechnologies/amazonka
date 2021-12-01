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
-- Module      : Amazonka.Lambda.Types.AccountUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.AccountUsage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The number of functions and amount of storage in use.
--
-- /See:/ 'newAccountUsage' smart constructor.
data AccountUsage = AccountUsage'
  { -- | The amount of storage space, in bytes, that\'s being used by deployment
    -- packages and layer archives.
    totalCodeSize :: Prelude.Maybe Prelude.Integer,
    -- | The number of Lambda functions.
    functionCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalCodeSize', 'accountUsage_totalCodeSize' - The amount of storage space, in bytes, that\'s being used by deployment
-- packages and layer archives.
--
-- 'functionCount', 'accountUsage_functionCount' - The number of Lambda functions.
newAccountUsage ::
  AccountUsage
newAccountUsage =
  AccountUsage'
    { totalCodeSize = Prelude.Nothing,
      functionCount = Prelude.Nothing
    }

-- | The amount of storage space, in bytes, that\'s being used by deployment
-- packages and layer archives.
accountUsage_totalCodeSize :: Lens.Lens' AccountUsage (Prelude.Maybe Prelude.Integer)
accountUsage_totalCodeSize = Lens.lens (\AccountUsage' {totalCodeSize} -> totalCodeSize) (\s@AccountUsage' {} a -> s {totalCodeSize = a} :: AccountUsage)

-- | The number of Lambda functions.
accountUsage_functionCount :: Lens.Lens' AccountUsage (Prelude.Maybe Prelude.Integer)
accountUsage_functionCount = Lens.lens (\AccountUsage' {functionCount} -> functionCount) (\s@AccountUsage' {} a -> s {functionCount = a} :: AccountUsage)

instance Core.FromJSON AccountUsage where
  parseJSON =
    Core.withObject
      "AccountUsage"
      ( \x ->
          AccountUsage'
            Prelude.<$> (x Core..:? "TotalCodeSize")
            Prelude.<*> (x Core..:? "FunctionCount")
      )

instance Prelude.Hashable AccountUsage where
  hashWithSalt salt' AccountUsage' {..} =
    salt' `Prelude.hashWithSalt` functionCount
      `Prelude.hashWithSalt` totalCodeSize

instance Prelude.NFData AccountUsage where
  rnf AccountUsage' {..} =
    Prelude.rnf totalCodeSize
      `Prelude.seq` Prelude.rnf functionCount
