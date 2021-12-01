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
-- Module      : Amazonka.SageMaker.Types.ProvisioningParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProvisioningParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A key value pair used when you provision a project as a service catalog
-- product. For information, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is Amazon Web Services Service Catalog>.
--
-- /See:/ 'newProvisioningParameter' smart constructor.
data ProvisioningParameter = ProvisioningParameter'
  { -- | The value of the provisioning parameter.
    value :: Prelude.Maybe Prelude.Text,
    -- | The key that identifies a provisioning parameter.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisioningParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'provisioningParameter_value' - The value of the provisioning parameter.
--
-- 'key', 'provisioningParameter_key' - The key that identifies a provisioning parameter.
newProvisioningParameter ::
  ProvisioningParameter
newProvisioningParameter =
  ProvisioningParameter'
    { value = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The value of the provisioning parameter.
provisioningParameter_value :: Lens.Lens' ProvisioningParameter (Prelude.Maybe Prelude.Text)
provisioningParameter_value = Lens.lens (\ProvisioningParameter' {value} -> value) (\s@ProvisioningParameter' {} a -> s {value = a} :: ProvisioningParameter)

-- | The key that identifies a provisioning parameter.
provisioningParameter_key :: Lens.Lens' ProvisioningParameter (Prelude.Maybe Prelude.Text)
provisioningParameter_key = Lens.lens (\ProvisioningParameter' {key} -> key) (\s@ProvisioningParameter' {} a -> s {key = a} :: ProvisioningParameter)

instance Core.FromJSON ProvisioningParameter where
  parseJSON =
    Core.withObject
      "ProvisioningParameter"
      ( \x ->
          ProvisioningParameter'
            Prelude.<$> (x Core..:? "Value") Prelude.<*> (x Core..:? "Key")
      )

instance Prelude.Hashable ProvisioningParameter where
  hashWithSalt salt' ProvisioningParameter' {..} =
    salt' `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData ProvisioningParameter where
  rnf ProvisioningParameter' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf key

instance Core.ToJSON ProvisioningParameter where
  toJSON ProvisioningParameter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            ("Key" Core..=) Prelude.<$> key
          ]
      )
