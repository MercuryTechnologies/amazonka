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
-- Module      : Amazonka.Glacier.Types.Grant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.Grant where

import qualified Amazonka.Core as Core
import Amazonka.Glacier.Types.Grantee
import Amazonka.Glacier.Types.Permission
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a grant.
--
-- /See:/ 'newGrant' smart constructor.
data Grant = Grant'
  { -- | Specifies the permission given to the grantee.
    permission :: Prelude.Maybe Permission,
    -- | The grantee.
    grantee :: Prelude.Maybe Grantee
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Grant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permission', 'grant_permission' - Specifies the permission given to the grantee.
--
-- 'grantee', 'grant_grantee' - The grantee.
newGrant ::
  Grant
newGrant =
  Grant'
    { permission = Prelude.Nothing,
      grantee = Prelude.Nothing
    }

-- | Specifies the permission given to the grantee.
grant_permission :: Lens.Lens' Grant (Prelude.Maybe Permission)
grant_permission = Lens.lens (\Grant' {permission} -> permission) (\s@Grant' {} a -> s {permission = a} :: Grant)

-- | The grantee.
grant_grantee :: Lens.Lens' Grant (Prelude.Maybe Grantee)
grant_grantee = Lens.lens (\Grant' {grantee} -> grantee) (\s@Grant' {} a -> s {grantee = a} :: Grant)

instance Core.FromJSON Grant where
  parseJSON =
    Core.withObject
      "Grant"
      ( \x ->
          Grant'
            Prelude.<$> (x Core..:? "Permission")
            Prelude.<*> (x Core..:? "Grantee")
      )

instance Prelude.Hashable Grant where
  hashWithSalt salt' Grant' {..} =
    salt' `Prelude.hashWithSalt` grantee
      `Prelude.hashWithSalt` permission

instance Prelude.NFData Grant where
  rnf Grant' {..} =
    Prelude.rnf permission
      `Prelude.seq` Prelude.rnf grantee

instance Core.ToJSON Grant where
  toJSON Grant' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Permission" Core..=) Prelude.<$> permission,
            ("Grantee" Core..=) Prelude.<$> grantee
          ]
      )
