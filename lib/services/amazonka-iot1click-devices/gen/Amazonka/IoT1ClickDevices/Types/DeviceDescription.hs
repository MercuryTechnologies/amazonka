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
-- Module      : Amazonka.IoT1ClickDevices.Types.DeviceDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT1ClickDevices.Types.DeviceDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newDeviceDescription' smart constructor.
data DeviceDescription = DeviceDescription'
  { -- | A value between 0 and 1 inclusive, representing the fraction of life
    -- remaining for the device.
    remainingLife :: Prelude.Maybe Prelude.Double,
    -- | A Boolean value indicating whether or not the device is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the device.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An array of zero or more elements of DeviceAttribute objects providing
    -- user specified device attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique identifier of the device.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The type of the device, such as \"button\".
    type' :: Prelude.Maybe Prelude.Text,
    -- | The tags currently associated with the AWS IoT 1-Click device.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remainingLife', 'deviceDescription_remainingLife' - A value between 0 and 1 inclusive, representing the fraction of life
-- remaining for the device.
--
-- 'enabled', 'deviceDescription_enabled' - A Boolean value indicating whether or not the device is enabled.
--
-- 'arn', 'deviceDescription_arn' - The ARN of the device.
--
-- 'attributes', 'deviceDescription_attributes' - An array of zero or more elements of DeviceAttribute objects providing
-- user specified device attributes.
--
-- 'deviceId', 'deviceDescription_deviceId' - The unique identifier of the device.
--
-- 'type'', 'deviceDescription_type' - The type of the device, such as \"button\".
--
-- 'tags', 'deviceDescription_tags' - The tags currently associated with the AWS IoT 1-Click device.
newDeviceDescription ::
  DeviceDescription
newDeviceDescription =
  DeviceDescription'
    { remainingLife = Prelude.Nothing,
      enabled = Prelude.Nothing,
      arn = Prelude.Nothing,
      attributes = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      type' = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | A value between 0 and 1 inclusive, representing the fraction of life
-- remaining for the device.
deviceDescription_remainingLife :: Lens.Lens' DeviceDescription (Prelude.Maybe Prelude.Double)
deviceDescription_remainingLife = Lens.lens (\DeviceDescription' {remainingLife} -> remainingLife) (\s@DeviceDescription' {} a -> s {remainingLife = a} :: DeviceDescription)

-- | A Boolean value indicating whether or not the device is enabled.
deviceDescription_enabled :: Lens.Lens' DeviceDescription (Prelude.Maybe Prelude.Bool)
deviceDescription_enabled = Lens.lens (\DeviceDescription' {enabled} -> enabled) (\s@DeviceDescription' {} a -> s {enabled = a} :: DeviceDescription)

-- | The ARN of the device.
deviceDescription_arn :: Lens.Lens' DeviceDescription (Prelude.Maybe Prelude.Text)
deviceDescription_arn = Lens.lens (\DeviceDescription' {arn} -> arn) (\s@DeviceDescription' {} a -> s {arn = a} :: DeviceDescription)

-- | An array of zero or more elements of DeviceAttribute objects providing
-- user specified device attributes.
deviceDescription_attributes :: Lens.Lens' DeviceDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
deviceDescription_attributes = Lens.lens (\DeviceDescription' {attributes} -> attributes) (\s@DeviceDescription' {} a -> s {attributes = a} :: DeviceDescription) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the device.
deviceDescription_deviceId :: Lens.Lens' DeviceDescription (Prelude.Maybe Prelude.Text)
deviceDescription_deviceId = Lens.lens (\DeviceDescription' {deviceId} -> deviceId) (\s@DeviceDescription' {} a -> s {deviceId = a} :: DeviceDescription)

-- | The type of the device, such as \"button\".
deviceDescription_type :: Lens.Lens' DeviceDescription (Prelude.Maybe Prelude.Text)
deviceDescription_type = Lens.lens (\DeviceDescription' {type'} -> type') (\s@DeviceDescription' {} a -> s {type' = a} :: DeviceDescription)

-- | The tags currently associated with the AWS IoT 1-Click device.
deviceDescription_tags :: Lens.Lens' DeviceDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
deviceDescription_tags = Lens.lens (\DeviceDescription' {tags} -> tags) (\s@DeviceDescription' {} a -> s {tags = a} :: DeviceDescription) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON DeviceDescription where
  parseJSON =
    Core.withObject
      "DeviceDescription"
      ( \x ->
          DeviceDescription'
            Prelude.<$> (x Core..:? "remainingLife")
            Prelude.<*> (x Core..:? "enabled")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "deviceId")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable DeviceDescription where
  hashWithSalt salt' DeviceDescription' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` remainingLife

instance Prelude.NFData DeviceDescription where
  rnf DeviceDescription' {..} =
    Prelude.rnf remainingLife
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf enabled
