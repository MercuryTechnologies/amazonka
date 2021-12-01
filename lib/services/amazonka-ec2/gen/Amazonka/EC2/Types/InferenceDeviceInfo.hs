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
-- Module      : Amazonka.EC2.Types.InferenceDeviceInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InferenceDeviceInfo where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the Inference accelerators for the instance type.
--
-- /See:/ 'newInferenceDeviceInfo' smart constructor.
data InferenceDeviceInfo = InferenceDeviceInfo'
  { -- | The manufacturer of the Inference accelerator.
    manufacturer :: Prelude.Maybe Prelude.Text,
    -- | The number of Inference accelerators for the instance type.
    count :: Prelude.Maybe Prelude.Int,
    -- | The name of the Inference accelerator.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceDeviceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manufacturer', 'inferenceDeviceInfo_manufacturer' - The manufacturer of the Inference accelerator.
--
-- 'count', 'inferenceDeviceInfo_count' - The number of Inference accelerators for the instance type.
--
-- 'name', 'inferenceDeviceInfo_name' - The name of the Inference accelerator.
newInferenceDeviceInfo ::
  InferenceDeviceInfo
newInferenceDeviceInfo =
  InferenceDeviceInfo'
    { manufacturer =
        Prelude.Nothing,
      count = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The manufacturer of the Inference accelerator.
inferenceDeviceInfo_manufacturer :: Lens.Lens' InferenceDeviceInfo (Prelude.Maybe Prelude.Text)
inferenceDeviceInfo_manufacturer = Lens.lens (\InferenceDeviceInfo' {manufacturer} -> manufacturer) (\s@InferenceDeviceInfo' {} a -> s {manufacturer = a} :: InferenceDeviceInfo)

-- | The number of Inference accelerators for the instance type.
inferenceDeviceInfo_count :: Lens.Lens' InferenceDeviceInfo (Prelude.Maybe Prelude.Int)
inferenceDeviceInfo_count = Lens.lens (\InferenceDeviceInfo' {count} -> count) (\s@InferenceDeviceInfo' {} a -> s {count = a} :: InferenceDeviceInfo)

-- | The name of the Inference accelerator.
inferenceDeviceInfo_name :: Lens.Lens' InferenceDeviceInfo (Prelude.Maybe Prelude.Text)
inferenceDeviceInfo_name = Lens.lens (\InferenceDeviceInfo' {name} -> name) (\s@InferenceDeviceInfo' {} a -> s {name = a} :: InferenceDeviceInfo)

instance Core.FromXML InferenceDeviceInfo where
  parseXML x =
    InferenceDeviceInfo'
      Prelude.<$> (x Core..@? "manufacturer")
      Prelude.<*> (x Core..@? "count")
      Prelude.<*> (x Core..@? "name")

instance Prelude.Hashable InferenceDeviceInfo where
  hashWithSalt salt' InferenceDeviceInfo' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` manufacturer

instance Prelude.NFData InferenceDeviceInfo where
  rnf InferenceDeviceInfo' {..} =
    Prelude.rnf manufacturer
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf count
