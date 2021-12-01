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
-- Module      : Amazonka.ElastiCache.Types.CacheNodeTypeSpecificParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.CacheNodeTypeSpecificParameter where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types.CacheNodeTypeSpecificValue
import Amazonka.ElastiCache.Types.ChangeType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A parameter that has a different value for each cache node type it is
-- applied to. For example, in a Redis cluster, a @cache.m1.large@ cache
-- node type would have a larger @maxmemory@ value than a @cache.m1.small@
-- type.
--
-- /See:/ 'newCacheNodeTypeSpecificParameter' smart constructor.
data CacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter'
  { -- | A list of cache node types and their corresponding values for this
    -- parameter.
    cacheNodeTypeSpecificValues :: Prelude.Maybe [CacheNodeTypeSpecificValue],
    -- | The earliest cache engine version to which the parameter can apply.
    minimumEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | The source of the parameter value.
    source :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether (@true@) or not (@false@) the parameter can be
    -- modified. Some parameters have security or operational implications that
    -- prevent them from being changed.
    isModifiable :: Prelude.Maybe Prelude.Bool,
    -- | The valid data type for the parameter.
    dataType :: Prelude.Maybe Prelude.Text,
    -- | The valid range of values for the parameter.
    allowedValues :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | A description of the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a change to the parameter is applied immediately or
    -- requires a reboot for the change to be applied. You can force a reboot
    -- or wait until the next maintenance window\'s reboot. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster>.
    changeType :: Prelude.Maybe ChangeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CacheNodeTypeSpecificParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheNodeTypeSpecificValues', 'cacheNodeTypeSpecificParameter_cacheNodeTypeSpecificValues' - A list of cache node types and their corresponding values for this
-- parameter.
--
-- 'minimumEngineVersion', 'cacheNodeTypeSpecificParameter_minimumEngineVersion' - The earliest cache engine version to which the parameter can apply.
--
-- 'source', 'cacheNodeTypeSpecificParameter_source' - The source of the parameter value.
--
-- 'isModifiable', 'cacheNodeTypeSpecificParameter_isModifiable' - Indicates whether (@true@) or not (@false@) the parameter can be
-- modified. Some parameters have security or operational implications that
-- prevent them from being changed.
--
-- 'dataType', 'cacheNodeTypeSpecificParameter_dataType' - The valid data type for the parameter.
--
-- 'allowedValues', 'cacheNodeTypeSpecificParameter_allowedValues' - The valid range of values for the parameter.
--
-- 'parameterName', 'cacheNodeTypeSpecificParameter_parameterName' - The name of the parameter.
--
-- 'description', 'cacheNodeTypeSpecificParameter_description' - A description of the parameter.
--
-- 'changeType', 'cacheNodeTypeSpecificParameter_changeType' - Indicates whether a change to the parameter is applied immediately or
-- requires a reboot for the change to be applied. You can force a reboot
-- or wait until the next maintenance window\'s reboot. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster>.
newCacheNodeTypeSpecificParameter ::
  CacheNodeTypeSpecificParameter
newCacheNodeTypeSpecificParameter =
  CacheNodeTypeSpecificParameter'
    { cacheNodeTypeSpecificValues =
        Prelude.Nothing,
      minimumEngineVersion = Prelude.Nothing,
      source = Prelude.Nothing,
      isModifiable = Prelude.Nothing,
      dataType = Prelude.Nothing,
      allowedValues = Prelude.Nothing,
      parameterName = Prelude.Nothing,
      description = Prelude.Nothing,
      changeType = Prelude.Nothing
    }

-- | A list of cache node types and their corresponding values for this
-- parameter.
cacheNodeTypeSpecificParameter_cacheNodeTypeSpecificValues :: Lens.Lens' CacheNodeTypeSpecificParameter (Prelude.Maybe [CacheNodeTypeSpecificValue])
cacheNodeTypeSpecificParameter_cacheNodeTypeSpecificValues = Lens.lens (\CacheNodeTypeSpecificParameter' {cacheNodeTypeSpecificValues} -> cacheNodeTypeSpecificValues) (\s@CacheNodeTypeSpecificParameter' {} a -> s {cacheNodeTypeSpecificValues = a} :: CacheNodeTypeSpecificParameter) Prelude.. Lens.mapping Lens.coerced

-- | The earliest cache engine version to which the parameter can apply.
cacheNodeTypeSpecificParameter_minimumEngineVersion :: Lens.Lens' CacheNodeTypeSpecificParameter (Prelude.Maybe Prelude.Text)
cacheNodeTypeSpecificParameter_minimumEngineVersion = Lens.lens (\CacheNodeTypeSpecificParameter' {minimumEngineVersion} -> minimumEngineVersion) (\s@CacheNodeTypeSpecificParameter' {} a -> s {minimumEngineVersion = a} :: CacheNodeTypeSpecificParameter)

-- | The source of the parameter value.
cacheNodeTypeSpecificParameter_source :: Lens.Lens' CacheNodeTypeSpecificParameter (Prelude.Maybe Prelude.Text)
cacheNodeTypeSpecificParameter_source = Lens.lens (\CacheNodeTypeSpecificParameter' {source} -> source) (\s@CacheNodeTypeSpecificParameter' {} a -> s {source = a} :: CacheNodeTypeSpecificParameter)

-- | Indicates whether (@true@) or not (@false@) the parameter can be
-- modified. Some parameters have security or operational implications that
-- prevent them from being changed.
cacheNodeTypeSpecificParameter_isModifiable :: Lens.Lens' CacheNodeTypeSpecificParameter (Prelude.Maybe Prelude.Bool)
cacheNodeTypeSpecificParameter_isModifiable = Lens.lens (\CacheNodeTypeSpecificParameter' {isModifiable} -> isModifiable) (\s@CacheNodeTypeSpecificParameter' {} a -> s {isModifiable = a} :: CacheNodeTypeSpecificParameter)

-- | The valid data type for the parameter.
cacheNodeTypeSpecificParameter_dataType :: Lens.Lens' CacheNodeTypeSpecificParameter (Prelude.Maybe Prelude.Text)
cacheNodeTypeSpecificParameter_dataType = Lens.lens (\CacheNodeTypeSpecificParameter' {dataType} -> dataType) (\s@CacheNodeTypeSpecificParameter' {} a -> s {dataType = a} :: CacheNodeTypeSpecificParameter)

-- | The valid range of values for the parameter.
cacheNodeTypeSpecificParameter_allowedValues :: Lens.Lens' CacheNodeTypeSpecificParameter (Prelude.Maybe Prelude.Text)
cacheNodeTypeSpecificParameter_allowedValues = Lens.lens (\CacheNodeTypeSpecificParameter' {allowedValues} -> allowedValues) (\s@CacheNodeTypeSpecificParameter' {} a -> s {allowedValues = a} :: CacheNodeTypeSpecificParameter)

-- | The name of the parameter.
cacheNodeTypeSpecificParameter_parameterName :: Lens.Lens' CacheNodeTypeSpecificParameter (Prelude.Maybe Prelude.Text)
cacheNodeTypeSpecificParameter_parameterName = Lens.lens (\CacheNodeTypeSpecificParameter' {parameterName} -> parameterName) (\s@CacheNodeTypeSpecificParameter' {} a -> s {parameterName = a} :: CacheNodeTypeSpecificParameter)

-- | A description of the parameter.
cacheNodeTypeSpecificParameter_description :: Lens.Lens' CacheNodeTypeSpecificParameter (Prelude.Maybe Prelude.Text)
cacheNodeTypeSpecificParameter_description = Lens.lens (\CacheNodeTypeSpecificParameter' {description} -> description) (\s@CacheNodeTypeSpecificParameter' {} a -> s {description = a} :: CacheNodeTypeSpecificParameter)

-- | Indicates whether a change to the parameter is applied immediately or
-- requires a reboot for the change to be applied. You can force a reboot
-- or wait until the next maintenance window\'s reboot. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster>.
cacheNodeTypeSpecificParameter_changeType :: Lens.Lens' CacheNodeTypeSpecificParameter (Prelude.Maybe ChangeType)
cacheNodeTypeSpecificParameter_changeType = Lens.lens (\CacheNodeTypeSpecificParameter' {changeType} -> changeType) (\s@CacheNodeTypeSpecificParameter' {} a -> s {changeType = a} :: CacheNodeTypeSpecificParameter)

instance Core.FromXML CacheNodeTypeSpecificParameter where
  parseXML x =
    CacheNodeTypeSpecificParameter'
      Prelude.<$> ( x Core..@? "CacheNodeTypeSpecificValues"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "CacheNodeTypeSpecificValue")
                  )
      Prelude.<*> (x Core..@? "MinimumEngineVersion")
      Prelude.<*> (x Core..@? "Source")
      Prelude.<*> (x Core..@? "IsModifiable")
      Prelude.<*> (x Core..@? "DataType")
      Prelude.<*> (x Core..@? "AllowedValues")
      Prelude.<*> (x Core..@? "ParameterName")
      Prelude.<*> (x Core..@? "Description")
      Prelude.<*> (x Core..@? "ChangeType")

instance
  Prelude.Hashable
    CacheNodeTypeSpecificParameter
  where
  hashWithSalt
    salt'
    CacheNodeTypeSpecificParameter' {..} =
      salt' `Prelude.hashWithSalt` changeType
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` parameterName
        `Prelude.hashWithSalt` allowedValues
        `Prelude.hashWithSalt` dataType
        `Prelude.hashWithSalt` isModifiable
        `Prelude.hashWithSalt` source
        `Prelude.hashWithSalt` minimumEngineVersion
        `Prelude.hashWithSalt` cacheNodeTypeSpecificValues

instance
  Prelude.NFData
    CacheNodeTypeSpecificParameter
  where
  rnf CacheNodeTypeSpecificParameter' {..} =
    Prelude.rnf cacheNodeTypeSpecificValues
      `Prelude.seq` Prelude.rnf changeType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf isModifiable
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf minimumEngineVersion
