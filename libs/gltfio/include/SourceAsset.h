/*
 * Copyright (C) 2022 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <stdint.h>
#include <stddef.h>

namespace gltfio {

    // This could perhaps be replaced with std::span in c++20, but we want a specific memory layout.
    template <typename T>
    struct span {
        T* const data;
        const size_t size;
    };

    // Similar to cgltf, except:
    // - field names are camelCase instead of snake_case to be consistent with the JSON
    // - extensions also use the name as it appears in the JSON, e.g. "KHR_draco_mesh_compression"
    // - no foo / foo_count pairs in favor of <foo>
    // - no "has" in favor of pointers or zero-length spans
    // - no unparsed "extensions" fields

    struct SourceAsset {
        struct Material; // TBD
        enum class FileType {
            INVALID,
            GLTF,
            GLB,
        };
        enum class PrimitiveType {
            POINTS,
            LINES,
            LINE_LOOP,
            LINE_STRIP,
            TRIANGLES,
            TRIANGLE_FAN,
            TRIANGLE_FTRIP,
        };
        enum class ComponentType {
            invalid,
            R_8,   // BYTE
            R_8U,  // UNSIGNED_BYTE
            R_16,  // SHORT
            R_16U, // UNSIGNED_SHORT
            R_32U, // UNSIGNED_INT
            R_32F, // FLOAT
        };
        enum class Type {
            INVALID,
            SCALAR,
            VEC2,
            VEC3,
            VEC4,
            MAT2,
            MAT3,
            MAT4,
        };
        enum class BufferViewType {
            INVALID,
            INDICES,
            VERTICES,
        };
        enum class AttributeType {
            INVALID,
            POSITION,
            NORMAL,
            TANGENT,
            TEXCOORD,
            COLOR,
            JOINTS,
            WEIGHTS,
        };
        struct Extras {
            size_t startOffset;
            size_t endOffset;
        };
        struct Extension {
            char* name;
            char* data;
        };
        struct Buffer {
            char* name;
            size_t size;
            char* uri;
            void* data;
            Extras extras;
        };
        struct BufferView
        {
            char *name;
            Buffer* buffer;
            size_t offset;
            size_t size;
            size_t stride; /* 0 == automatically determined by accessor */
            BufferViewType type;
            Extras extras;
        };
        struct AccessorSparse {
            size_t count;
            BufferView* indices_buffer_view;
            size_t indices_byte_offset;
            ComponentType indices_component_type;
            BufferView* values_buffer_view;
            size_t values_byte_offset;
            Extras extras;
            Extras indicesExtras;
            Extras valuesExtras;
        };
        struct Accessor {
            char* name;
            ComponentType componentType;
            bool normalized;
            Type type;
            size_t offset;
            size_t count;
            size_t stride;
            BufferView* bufferView;
            span<float> min;
            span<float> max;
            AccessorSparse* sparse;
            Extras extras;
        };
        struct Attribute {
            char* name;
            AttributeType type;
            size_t index;
            Accessor* data;
        };
        struct MaterialVariantMapping {
            size_t variant;
            Material* material;
            Extras extras;
        };
        struct DracoMeshCompression {
            BufferView* buffer_view;
            span<Attribute> attributes;
        };
        struct Primitive {
            PrimitiveType type;
            span<Accessor> indices;
            span<Material> material;
            span<Attribute> attributes;
            span<Attribute> targets;
            Extras extras;
            DracoMeshCompression* KHR_draco_mesh_compression;
            span<MaterialVariantMapping> KHR_materials_variants;
        };
        struct Mesh {
            char* name;
            span<Primitive> primitives;
            span<float> weights;
            span<char*> targetNames;
            Extras extras;
        };
        struct Asset {
            char* copyright;
            char* generator;
            char* version;
            char* minVersion;
            Extras extras;
        };
        FileType fileType;
        Asset assset;
        span<Mesh> meshes;
    };

} // namespace gltfio
