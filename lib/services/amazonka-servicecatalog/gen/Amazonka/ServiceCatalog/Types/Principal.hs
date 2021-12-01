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
-- Module      : Amazonka.ServiceCatalog.Types.Principal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.Principal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.PrincipalType

-- | Information about a principal.
--
-- /See:/ 'newPrincipal' smart constructor.
data Principal = Principal'
  { -- | The principal type. The supported value is @IAM@.
    principalType :: Prelude.Maybe PrincipalType,
    -- | The ARN of the principal (IAM user, role, or group).
    principalARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Principal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalType', 'principal_principalType' - The principal type. The supported value is @IAM@.
--
-- 'principalARN', 'principal_principalARN' - The ARN of the principal (IAM user, role, or group).
newPrincipal ::
  Principal
newPrincipal =
  Principal'
    { principalType = Prelude.Nothing,
      principalARN = Prelude.Nothing
    }

-- | The principal type. The supported value is @IAM@.
principal_principalType :: Lens.Lens' Principal (Prelude.Maybe PrincipalType)
principal_principalType = Lens.lens (\Principal' {principalType} -> principalType) (\s@Principal' {} a -> s {principalType = a} :: Principal)

-- | The ARN of the principal (IAM user, role, or group).
principal_principalARN :: Lens.Lens' Principal (Prelude.Maybe Prelude.Text)
principal_principalARN = Lens.lens (\Principal' {principalARN} -> principalARN) (\s@Principal' {} a -> s {principalARN = a} :: Principal)

instance Core.FromJSON Principal where
  parseJSON =
    Core.withObject
      "Principal"
      ( \x ->
          Principal'
            Prelude.<$> (x Core..:? "PrincipalType")
            Prelude.<*> (x Core..:? "PrincipalARN")
      )

instance Prelude.Hashable Principal where
  hashWithSalt salt' Principal' {..} =
    salt' `Prelude.hashWithSalt` principalARN
      `Prelude.hashWithSalt` principalType

instance Prelude.NFData Principal where
  rnf Principal' {..} =
    Prelude.rnf principalType
      `Prelude.seq` Prelude.rnf principalARN
