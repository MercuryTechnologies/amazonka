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
-- Module      : Amazonka.Config.Types.RemediationParameterValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.RemediationParameterValue where

import Amazonka.Config.Types.ResourceValue
import Amazonka.Config.Types.StaticValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The value is either a dynamic (resource) value or a static value. You
-- must select either a dynamic value or a static value.
--
-- /See:/ 'newRemediationParameterValue' smart constructor.
data RemediationParameterValue = RemediationParameterValue'
  { -- | The value is static and does not change at run-time.
    staticValue :: Prelude.Maybe StaticValue,
    -- | The value is dynamic and changes at run-time.
    resourceValue :: Prelude.Maybe ResourceValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemediationParameterValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticValue', 'remediationParameterValue_staticValue' - The value is static and does not change at run-time.
--
-- 'resourceValue', 'remediationParameterValue_resourceValue' - The value is dynamic and changes at run-time.
newRemediationParameterValue ::
  RemediationParameterValue
newRemediationParameterValue =
  RemediationParameterValue'
    { staticValue =
        Prelude.Nothing,
      resourceValue = Prelude.Nothing
    }

-- | The value is static and does not change at run-time.
remediationParameterValue_staticValue :: Lens.Lens' RemediationParameterValue (Prelude.Maybe StaticValue)
remediationParameterValue_staticValue = Lens.lens (\RemediationParameterValue' {staticValue} -> staticValue) (\s@RemediationParameterValue' {} a -> s {staticValue = a} :: RemediationParameterValue)

-- | The value is dynamic and changes at run-time.
remediationParameterValue_resourceValue :: Lens.Lens' RemediationParameterValue (Prelude.Maybe ResourceValue)
remediationParameterValue_resourceValue = Lens.lens (\RemediationParameterValue' {resourceValue} -> resourceValue) (\s@RemediationParameterValue' {} a -> s {resourceValue = a} :: RemediationParameterValue)

instance Core.FromJSON RemediationParameterValue where
  parseJSON =
    Core.withObject
      "RemediationParameterValue"
      ( \x ->
          RemediationParameterValue'
            Prelude.<$> (x Core..:? "StaticValue")
            Prelude.<*> (x Core..:? "ResourceValue")
      )

instance Prelude.Hashable RemediationParameterValue where
  hashWithSalt salt' RemediationParameterValue' {..} =
    salt' `Prelude.hashWithSalt` resourceValue
      `Prelude.hashWithSalt` staticValue

instance Prelude.NFData RemediationParameterValue where
  rnf RemediationParameterValue' {..} =
    Prelude.rnf staticValue
      `Prelude.seq` Prelude.rnf resourceValue

instance Core.ToJSON RemediationParameterValue where
  toJSON RemediationParameterValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StaticValue" Core..=) Prelude.<$> staticValue,
            ("ResourceValue" Core..=) Prelude.<$> resourceValue
          ]
      )
